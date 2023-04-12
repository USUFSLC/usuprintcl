(defpackage usuprintcl.app
  (:use :cl)
  (:import-from :lack.middleware.session.state.cookie
                :make-cookie-state)
  (:export #:start #:stop))
(in-package :usuprintcl.app)

;; Globals
;; I swear I have a license https://regexlicensing.org/
(defparameter *BFT-REGEX* "^https:\/\/bft.usu.edu\/[a-zA-Z0-9]+$")
(defparameter *A-NUMBER-REGEX* "^A[0-9]{8}$")

(defparameter *JWT_VALIDATE_ANUMBER_EXPR_SEC* (* 60 60 3)) ;; 3 hours
(defparameter *COOKIE_EXPIRE_SEC* (* 60 60 24))

(defparameter *CUPS-HOST* "vmpps4.aggies.usu.edu")
(defparameter *COLOR-PATH* '(("monochrome" . "Campus-BW")
                             ("color" . "Campus-Color")))
(defparameter *CUPS-OPTIONS* '((media .
                                (("a4" . "a4")
                                 ("letter" . "letter")
                                 ("legal" . "legal")))
                               (number-up .
                                (("1" . "")
                                 ("2" . "2")
                                 ("4" . "4")
                                 ("6" . "6")
                                 ("9" . "9")
                                 ("16" . "16")))
                               (orientation-requested .
                                (("No Orientation" . "")
                                 ("90 Deg. Counter Clockwise" . "4")
                                 ("90 Deg. Clockwise" . "5")
                                 ("180 Deg." . "6")))
                               (sides .
                                (("One Sided" . "one-sided")
                                 ("Two Sided (Portrait)" . "two-sided-long-edge")
                                 ("Two Sided (Landscape)" . "two-sided-short-edge")))))

(defparameter *DEFAULT-CUPS-OPTIONS* '((media . "a4")
                                       (number-up . "1")
                                       (orientation-request . "No Orientation")
                                       (sides . "One Sided")))

;; Conditions
(define-condition invalid-anumber (error)
  ())
(define-condition jwt-expiration (error)
  ())
(define-condition invalid-bft (error)
  ())

;; Signatures
(defvar *key*
  (ironclad:ascii-string-to-byte-array (uiop:getenv "JWT_SECRET")))

(defun validate-anumber-or-throw (anumber)
  (or (cl-ppcre:all-matches *A-NUMBER-REGEX* anumber)
      (error 'invalid-anumber)))

(defun sign-a-number (anumber &optional
                                (exp_sec *JWT_VALIDATE_ANUMBER_EXPR_SEC*))
  (validate-anumber-or-throw anumber)
  (jose:encode :hs256 *key*`(("anumber" .  anumber)
                             ("exp" . ,(+ (get-universal-time)
                                          exp_sec)))))

(defun valid-token-p-get-anumber (token)
  (let* ((decoded (jose:decode :hs256 *key* token))
         (expiration (cdr (assoc "exp" decoded :test #'string=)))
         (anumber (cdr (assoc "anumber" decoded :test #'string=))))
    (if (< (get-universal-time) (expiration))
        anumber
        (error 'jwt-expiration))))

;; Big File Transfer
(defun make-unique-pdf ()
  (format nil "/tmp/~a-~a.pdf" (get-universal-time) (random (expt 10 10))))

(defun validate-bft-or-throw (bft)
  (or (cl-ppcre:all-matches *BFT-REGEX* bft)
      (error 'invalid-bft)))

(defun save-bft (bft)
  (validate-bft-or-throw bft)
  (let ((pdf-path (make-unique-pdf)))
    (uiop:run-program "/usr/bin/wget ~a -O ~a" bft pdf-path)
    pdf-path))

(defun make-printer-uri (anumber &key (host *CUPS-HOST*) (color "monochrome"))
  (validate-anumber-or-throw anumber)
  (let ((lowered-anumber (string-downcase anumber)))
    (format nil "lpd://~a@~a/~a"
            lowered-anumber
            host
            (assoc color *COLOR-PATH* :test #'string))))

(defun make-cups-print-command (printer-uri title options-alist filename)
  (let ((o-options (reduce
                    (lambda (option-val options)
                      (let* ((option (car option-val))
                             (val (assoc
                                   (assoc *CUPS-OPTIONS* option) (cdr option-val)
                                   :test #'string=)))
                        (if (> (length val) 0)
                            (string-downcase
                             (concatenate 'string
                                          options
                                          (format nil " -o ~a=~a" option val)))
                            options)
                        options-alist)))))
    (format nil "/usr/bin/lp -h \"~a\" -T \"~a\" ~a ~a"
            printer-uri
            title
            o-options
            filename)))

;; Sendgrid

(defun set-session-from-token (env)
  (let* ((params (getf env :body-parameters))
         (token (cdr (assoc params "token" :test #'string=)))
         (session (getf env :lack.session)))
    (cond
      ((valid-token-p-get-anumber token)
       (setf (gethash :anumber session) token)
       '(200 (:content-type "text/plain")
         ("Dear Bob, you welcome!")))
      (t
       '(401 (:content-type "text/plain")
         ("Invalid or expired token"))))))

(defun job-form ()
  (cl-markup:html5
   (:form :method "post"
          :enctype "multipart/form-data"
          (:input :type "text"
                  :name "title")
          (:input :type "file"
                  :name "payload")
;;          (:select :name "color"
;;                   (:option :value "monochrome"
;;                            "Black and White")
;;                   (:option :value "color"
;;                            "Color"))
          (loop for options in *CUPS-OPTIONS*
                collect
                (let ((option (car options))
                      (option-selections (mapcar #'car (cdr options))))
                  (cl-markup:markup (:select :name option
                                   (loop for val in option-selections
                                         collect
                                         (cl-markup:markup*
                                          `(:option :name ,val
                                                    ,val)))))))
	        (:input :type "submit"))))

(defun copy-binary-stream-to-file (stream path)
  (with-open-file (outstream path
                             :direction :output
                             :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte stream nil)
          while byte
          do (write-byte byte outstream))))

(defun print-job (env)
  (case (getf env :request-method)
    (:get
     (list 200 nil (list (job-form))))
    (:post
     (let* ((params (getf env :body-parameters))
            (stream (cdr (assoc "payload" params :test #'string=)))
            (path (make-unique-pdf)))
       (format t "~a" stream)
       (copy-binary-stream-to-file
        (flexi-streams:make-flexi-stream stream
                                         :external-format :utf-8)
        path)
       (when (and
		          (typep stream 'file-stream)
		          (probe-file stream))
         (delete-file stream))
       (list 200 nil (list "Hello"))))))



;; And... GO!
(setf *app*
      (lack:builder
       `(:session
         :state
         ,(make-cookie-state
           :httponly t
           :secure t
           :expires *COOKIE_EXPIRE_SEC*))
       (:mount "/print" 'print-job)
       (:mount "/token" 'set-session-from-token)
       (lambda (env)
         `(200
           (:content-type "text/plain")
           ("Hello")))))

(defun start ()
  (clack:clackup *app*
                 :port 4000
                 :address "0.0.0.0"))

;; Stop
(defun stop ()
  (clack:stop *app*))


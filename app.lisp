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
(defparameter *COLOR-PATH* '((monochrome . "Campus-BW")
                             (color . "Campus-Color")))
(defparameter *CUPS-OPTIONS* '((media .
                                (("a4" . "a4")
                                 ("letter" . "letter")
                                 ("legal" . "legal")))
                               (number-up .
                                (("2" . "2")
                                 ("4" . "4")
                                 ("6" . "6")
                                 ("9" . "9")
                                 ("16" . "16")))
                               (orientation-requested .
                                (("90 Deg. Counter Clockwise " . "4")
                                 ("90 Deg. Clockwise" . "5")
                                 ("180 Deg." . "6")))
                               (sides .
                                (("One Sided" . "one-sided")
                                 ("Two Sided (Portrait)" . "two-sided-long-edge")
                                 ("Two Sided (Landscape)" . "two-sided-short-edge")))))
(defparameter *DEFAULT-CUPS-OPTIONS* '((media . "a4")
                                       (number-up . nil)
                                       (orientation-request . nil)
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
  (let* ((decoded (jose:decode :hs256 *key*
                               token))
         (expiration (assoc "exp" decoded))
         (anumber (assoc "anumber" decoded)))
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
            (assoc color *COLOR-PATH*))))

(defun make-cups-print-command (printer-uri title options-alist filename)
  (let ((o-options (reduce
                    (lambda (option-val options)
                      (let* ((option (car option-val))
                             (val (assoc
                                   (assoc *CUPS-OPTIONS* option)
                                   (cdr option-val))))
                        (if val
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

;; TODO: Put in email
(defun request-token (env)
  (validate-anumber-or-throw)
  `(200 :content-type "text-plain"
        (,(sign-a-number anumber))))

(defun set-session-from-token (env)
  (let* ((params (getf env :body-parameters))
         (token (alexandria:assoc-value
                 params
                 "token" :test #'string=))
         (session (getf env
                        :lack.session)))
    (cond
      ((valid-token-p-get-anumber token)
       (setf (gethash :anumber session) token)
       '(200 (:content-type "text/plain")
         ("Dear Bob, you welcome!")))
      (t
       '(401 (:content-type "text/plain")
         ("Invalid or expired token"))))))

(defun job-form ()
  (cl-who:with-html-output-to-string (s)
	  (:html
	   (:body
	    (:form :method "post"	           
		         :enctype "multipart/form-data"
             (:input :type "text"
		                 :name "title")
             (:select :name "color"
                      (:option :value "monochrome"
                               "Black and White")
                      (:option :value "color"
                               "Color"))
             (mapcar
              (lambda (options)
                (let ((option (car options))
                      (option-selections (mapcar #'car (cdr options))))
                  (htm
                   (:select :name option
                            (mapcar (lambda (val)
                                      (html (:option :name val
                                                     val)))
                                    option-selections)))))
              *CUPS-OPTIONS*)
             (:input :type "file"
		                 :name "payload")
	           (:input :type "submit"))))))

(defun print-job (env)
  (case (getf env :request-method)
    (:get
     `(200
       nil
       (,(job-form))))
    (:post
     (let* ((req (lack.request:make-request env))
            (body-params (lack.request:request-body-parameters req))) 
       (print env)
       (destructuring-bind (stream fname content-type)
	         (cdr (assoc "payload" body-params :test #'equal))
	       (when (and
		            (typep stream 'file-stream)
		            (probe-file stream))
	         (delete-file stream))	; see https://github.com/fukamachi/smart-buffer/issues/1
	       `(
	         200
	         nil
	         (,(format nil "~S ~S ~S ~S~%" env stream fname content-type))))))))


;; And... GO!
(setf *app*
      (lack:builder
       `(:session
         :state
         ,(make-cookie-state
           :httponly t
           :secure t
           :expires *COOKIE_EXPIRE_SEC*))
       (lambda (env)
         
         (format t "~a" env)
         '(200
           (:content-type "text/plain")
           ("Hello, World")))))

(defun start ()
  (clack:clackup *app*
                 :port 4000
                 :address "0.0.0.0"))

;; Stop
(defun stop ()
  (clack:stop *app*))

(defpackage usuprintcl.app
  (:use :cl)
  (:import-from :lack.middleware.session.state.cookie
   :make-cookie-state)
  (:import-from :lack.middleware.session.store.redis
   :make-redis-store)
  (:export #:start
           #:stop))
(in-package :usuprintcl.app)

;; Globals
;; I swear I have a license https://regexlicensing.org/
(defvar *A-NUMBER-REGEX* "^A[0-9]{8}$")
(defvar *REDIS-HOST* (uiop:getenv "REDIS_HOST"))
(defvar *key*
  (ironclad:ascii-string-to-byte-array (uiop:getenv "JWT_SECRET")))
(defvar *SENDGRID-API-KEY* (uiop:getenv "SENDGRID_API_KEY"))
(defvar *JWT-VALIDATE-ANUMBER-EXPIRE* (* 60 60 3)) ;; 3 hours
(defvar *COOKIE-EXPIRE-SEC* (* 60 60 24)) ;; A day

(defvar *FROM-EMAIL* (uiop:getenv "FROM_EMAIL"))
(defvar *PRINT-SERVER-HOST* (uiop:getenv "HOST"))
(defvar *SUBJECT-LINE* "FSLC Print Job Authentication")
(defvar *SENDGRID-SEND-PATH* "https://api.sendgrid.com/v3/mail/send")
(defvar *CUPS-HOST* "vmpps4.aggies.usu.edu")
(defvar *COLOR-PATH* '(("monochrome" . "Campus-BW")
                       ("color" . "Campus-Color")))
(defvar *CUPS-OPTIONS* '((media .
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

;; Conditions
(define-condition invalid-anumber (error)
  ())
(define-condition jwt-expiration (error)
  ())
(define-condition invalid-bft (error)
  ())

;; Templates
(defun job-form ()
  (cl-markup:html5
   (:form :method "post"
          :enctype "multipart/form-data"
          (:input :type "text"
                  :name "title")
          (:input :type "file"
                  :name "payload")
          (:select :name "color"
                   (:option :value "monochrome"
                            "Black and White")
                   (:option :value "color"
                            "Color"))
          (loop for options in *CUPS-OPTIONS*
                collect
                (let ((option (car options))
                      (option-selections (mapcar #'car (cdr options))))
                  (cl-markup:markup (:select :name (string-downcase (string option))
                                             (loop for val in option-selections
                                                   collect
                                                   (cl-markup:markup*
                                                    `(:option :name ,val
                                                              ,val)))))))
	        (:input :type "submit"))))

;; Signatures

(defun valid-anumber (anumber)
  (cl-ppcre:all-matches *A-NUMBER-REGEX* anumber))

(defun validate-anumber-or-throw (anumber)
  (or (valid-anumber anumber)
      (error 'invalid-anumber)))

(defun sign-a-number (anumber &optional
                                (exp_sec *JWT-VALIDATE-ANUMBER-EXPIRE*))
  (validate-anumber-or-throw anumber)
  (jose:encode :hs256 *key*
               `(("anumber" . ,anumber)
                 ("exp" . ,(+ (get-universal-time)
                              exp_sec)))))

(defun valid-token-p-get-anumber (token)
  (let* ((decoded (jose:decode :hs256 *key* token))
         (expiration (cdr (assoc "exp" decoded :test #'string=)))
         (anumber (cdr (assoc "anumber" decoded :test #'string=))))
    (if (< (get-universal-time) expiration)
        anumber
        (error 'jwt-expiration))))

;; CUPS

(defun make-unique-pdf ()
  (format nil "/tmp/~a-~a.pdf" (get-universal-time) (random (expt 10 10))))

(defun make-printer-uri (anumber &key (host *CUPS-HOST*) (color "monochrome"))
  (validate-anumber-or-throw anumber)
  (let ((lowered-anumber (string-downcase anumber)))
    (format nil "lpd://~a@~a/~a"
            lowered-anumber
            host
            (cdr (assoc color *COLOR-PATH* :test #'string=)))))

(defun make-cups-print-command (printer-uri filename &key title options-alist)
  (let ((o-options (reduce
                    (lambda (options option-val)
                      (let* ((option (car option-val))
                             (val (cdr (assoc (cdr option-val) (cdr (assoc option *CUPS-OPTIONS*)) :test #'string=))))
                        (if (> (length val) 0)
                            (string-downcase
                             (format nil "~a -o ~a=~a" options option val))
                            options)))
                    options-alist :initial-value "")))
    (format nil "/usr/bin/lp -h \"~a\" -T \"~a\" ~a ~a"
            printer-uri
            title
            o-options
            filename)))

;; Sendgrid

(defun make-sendgrid-email (subject from to msg)
  (cl-json:encode-json-to-string
   `(("from" . (("email" . ,from)))
     ("content" .
                ((("type" . "text/html")
                  ("value" . ,msg))))
     ("personalizations" . 

                         ((("subject" . ,subject)
                           ("to" .
                                 ((("email" . ,to))))))))))

(defun send-token-to-aggie (anumber token)
  (validate-anumber-or-throw anumber)
  (let* ((aggiemail (format nil "~a@usu.edu" anumber))
         (token-link (format nil "~a/auth?token=~a"
                             *PRINT-SERVER-HOST*
                             token))
         (body (format nil "Hello! Looks like you (~a) have submitted a print job to the FSLC print server. Please verify your identity using the following link: <a href=\"~a\">~a</a>"
                       anumber
                       token-link
                       token-link))
         (sendgrid-req-body (make-sendgrid-email
                                   *SUBJECT-LINE*
                                   *FROM-EMAIL*
                                   aggiemail
                                   body)))
    (drakma:http-request *SENDGRID-SEND-PATH*
                         :method :post
                         :content sendgrid-req-body
                         :additional-headers `(("Content-Type" . "application/json")
                                               ("Authorization" . ,(concatenate 'string "Bearer " *SENDGRID-API-KEY*))))))

(defun request-token (env)
  (let* ((params (getf env :query-parameters))
         (anumber (cdr (assoc "anumber" params :test #'string=))))
    (cond
      ((valid-anumber anumber)
       (send-token-to-aggie anumber (sign-a-number anumber))
       (list 200 '(:content-type "text/plain")
             (list (format nil "An email will soon be sent to ~a, please follow its instructions." anumber))))
      (t
       `(400 (:content-type "text/plain") ("Invalid anumber"))))))

(defun set-session-from-token (env)
  (let* ((params (getf env :query-parameters))
         (token (cdr (assoc "token" params :test #'string=)))
         (session (getf env :lack.session))
         (maybe-anumber (valid-token-p-get-anumber token)))
    (cond
      (maybe-anumber
       (setf (gethash :anumber session) maybe-anumber)
       (list 200 '(:content-type "text/plain")
             (list (concatenate 'string "Thank you, " maybe-anumber))))
      (t
       '(401 (:content-type "text/plain")
         ("Invalid or expired token"))))))

(defun copy-binary-stream-to-file (stream path)
  (with-open-file (filestream path
                              :direction :output
                              :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte stream nil)
          while byte
          do (write-byte byte filestream))))

(defun print-job (env)
  (case (getf env :request-method)
    (:get
     (list 200 '(:content-type "text/html")
           (list (job-form))))
    (:post
     (let* ((params (getf env :body-parameters))
            (session (getf env :lack.session))
            (anumber (gethash :anumber session))
            (file-stream (cadr (assoc "payload" params :test #'string=)))
            (color (cdr (assoc "color" params :test #'string=)))
            (title (cdr (assoc "title" params :test #'string=)))
            (path (make-unique-pdf)))
       (copy-binary-stream-to-file
        (flexi-streams:make-flexi-stream file-stream
                                         :external-format :utf-8)
        path)
       (when (and
		          (typep file-stream 'file-stream)
		          (probe-file file-stream))
	       (delete-file file-stream))
       (list 200 '(:content-type "text/plain")
             (list
              (make-cups-print-command
               (make-printer-uri anumber :color color)
               path
               :title title
               :options-alist (mapcar
                (lambda (option)
                  (cons
                   option
                   (cdr (assoc (string-downcase (string option))
                               params :test #'string=))))
                (mapcar #'car *CUPS-OPTIONS*)))))))))

;; And... GO!
(setf *app*
      (lack:builder
       `(:session
         :store
         ,(make-redis-store :namespace "session"
                            :host *REDIS-HOST*)
         :state
         ,(make-cookie-state
           :httponly t
           :secure t
           :expires *COOKIE-EXPIRE-SEC*))
       (:mount "/print" 'print-job)
       (:mount "/token" 'request-token)
       (:mount "/auth" 'set-session-from-token)
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


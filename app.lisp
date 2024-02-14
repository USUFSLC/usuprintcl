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
(defvar *A-NUMBER-REGEX* "^(A|a)[0-9]{8}$")
(defvar *SAFE-ARG-REGEX* "^[a-zA-Z0-9 \-_]*$")
(defvar *REDIS-HOST* (uiop:getenv "REDIS_HOST"))
(defvar *key*
  (ironclad:ascii-string-to-byte-array (uiop:getenv "JWT_SECRET")))
(defvar *AGGIE_AUTH_API_HOST* (uiop:getenv "AGGIE_AUTH_API_HOST"))
(defvar *AGGIE_AUTH_API_KEY* (uiop:getenv "AGGIE_AUTH_API_KEY"))
(defvar *JWT-VALIDATE-ANUMBER-EXPIRE* (* 60 60 3)) ;; 3 hours
(defvar *COOKIE-EXPIRE-SEC* (* 60 60 24)) ;; A day
(defvar *PPD-FILE* "generic-postscript.ppd")
(defvar *CUPS-HOST* "vmpps4.aggies.usu.edu")
(defvar *COLOR-PATH* '(("monochrome" . "Campus-BW")
                       ("color" . "Campus-Color")))
(defvar *CUPS-OPTIONS-DISPLAY-NAMES* '((media . "Paper Size")
                                       (number-up . "Copies Per Page")
                                       (orientation-requested . "Orientation")
                                       (sides . "Sides")))
(defvar *CUPS-OPTIONS* '((media .
                          (("letter" . "letter")
                           ("a4" . "a4")
                           ("legal" . "legal")
                           ))
                         (number-up .
                          (("1" . "")
                           ("2" . "2")
                           ("4" . "4")
                           ("6" . "6")
                           ("9" . "9")
                           ("16" . "16")))
                         (orientation-requested .
                          (("0 Deg." . "")
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

;; Templates
(defmacro render-env-with-root ((env) &body body)
  `(let* ((session (getf ,env :lack.session))
          (authenticated (gethash :anumber session))
          (message (gethash :message session)))
     (remhash :message session)
     (cl-markup:html5
       (:html
         (:head
           (:title "usuprintcl")
           (:link :rel "icon"
                  :type "image/png"
                  :href "/static/favicon-32x32.png")
           (:meta :name "viewport"
                  :content "width=device-width, initial-scale=1")
           (:meta :charset "UTF-8")
           (:link :rel "stylesheet"
                  :href "/static/pico.min.css")
           (:link :rel "stylesheet"
                  :href "/static/style.css"))
         (:body
           (:main :class "container"
                  (:div :class "header"
                        (:div :class "headings"
                              (:h1 "usuprintcl")
                              (:h4 "an *unofficial* USU printer job submission app - for those of us not running crapware on our crapboxes"))
                        (:div
                          (:a :href "https://github.com/USUFSLC/usuprintcl"
                              (:img :class "lisp-logo" :src "/static/lisp.png"))))
                  (:a :href "/logout"
                      (if authenticated
                        "Logout"))
                  (:hr)
                  (:div :class "message"
                        (if message
                          message))
                  ,@body))))))

(defun login-page (env)
  (render-env-with-root (env)
                        (:form :method "get"
                               :action "/token"
                               (:label :for "anumber"
                                       "A-Number")
                               (:input :name "anumber"
                                       :placeholder "A01234567"
                                       :type "text")
                               (:input :type "submit"))))

(defun four-oh-four (env)
  (render-env-with-root (env)
                        (:p "404 - Not found")))

(defun job-form (env)
  (render-env-with-root (env)
                        (:form :method "post"
                               :enctype "multipart/form-data"
                               (:label :for "title"
                                       "Job Name*")
                               (:input :type "text"
                                       :placeholder "Equation Sheet"
                                       :required t
                                       :name "title")
                               (:label :for "payload"
                                       "PDF File*")
                               (:input :type "file"
                                       :accept "application/pdf"
                                       :required t
                                       :name "payload")
                               (:label :for "copies"
                                       "Copies*")
                               (:input :type "number"
                                       :required t
                                       :value "1"
                                       :name "copies")
                               (:label :for "color"
                                       "Color*")
                               (:select :name "color"
                                        :required t
                                        (:option :value "monochrome"
                                                 "Black and White")
                                        (:option :value "color"
                                                 "Color"))
                               (loop for options in *CUPS-OPTIONS*
                                     collect
                                     (let* ((option (car options))
                                            (option-selections (mapcar #'car (cdr options)))
                                            (name (string-downcase (string option)))
                                            (display-name (cdr
                                                            (assoc option *CUPS-OPTIONS-DISPLAY-NAMES*))))
                                       (cl-markup:markup
                                         (:label :for name
                                                 (concatenate 'string display-name "*"))
                                         (:select :required t
                                                  :name name
                                                  (loop for val in option-selections
                                                        collect
                                                        (cl-markup:markup*
                                                          `(:option :value ,val
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
      anumber)))

;; CUPS

(defun make-unique-printer-name ()
  (format nil "~a-~a" (get-universal-time) (random (expt 10 10))))

(defun make-unique-pdf ()
  (format nil "/tmp/~a-~a.pdf" (get-universal-time) (random (expt 10 10))))

(defun make-printer-uri (anumber &key (host *CUPS-HOST*) (color "monochrome"))
  (validate-anumber-or-throw anumber)
  (let ((lowered-anumber (string-downcase anumber)))
    (format nil "lpd://~a@~a/~a"
            lowered-anumber
            host
            (cdr (assoc color *COLOR-PATH* :test #'string=)))))

(defun make-cups-create-printer-cmd (printer-name printer-uri &optional (ppd-file *PPD-FILE*))
  (format nil "lpadmin -P \"~a\" -p \"~a\" -E -v \"~a\""
          ppd-file
          printer-name
          printer-uri))

(defun make-cups-delete-printer-command (printer-name)
  (format nil "lpadmin -x \"~a\"" printer-name))

(defun make-cups-print-command (printer-name filename &key title options-alist (copies "1"))
  (let ((o-options (reduce
                     (lambda (options option-val)
                       (let* ((option (car option-val))
                              (val (cdr (assoc (cdr option-val) (cdr (assoc option *CUPS-OPTIONS*)) :test #'string=))))
                         (if (> (length val) 0)
                           (string-downcase
                             (format nil "~a -o ~a=~a" options option val))
                           options)))
                     options-alist :initial-value "")))
    (format nil "lp -d \"~a\" -t \"~a\" -n ~a ~a ~a"
            printer-name
            title
            copies
            o-options
            filename)))

;; Aggie-Auth & Auth

(defparameter *aggie-auth-tokens* (make-hash-table :test #'equal))

(defun send-token-to-aggie (anumber)
  (validate-anumber-or-throw anumber)
  (let* ((aggie-auth-resp
           (cl-json:decode-json
             (drakma:http-request (format nil "~a/authaggie" *AGGIE_AUTH_API_HOST*)
                                  :want-stream t
                                  :method :post
                                  :parameters `(("anumber" . ,(string-downcase anumber)))
                                  :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" *AGGIE_AUTH_API_KEY*))))))
         (token (cdr (assoc ':token aggie-auth-resp))))

    (setf (gethash token *aggie-auth-tokens*)
          (sign-a-number anumber))))

(defun request-token (env)
  (let* ((params (getf env :query-parameters))
         (anumber (cdr (assoc "anumber" params :test #'string=)))
         (message
           (format nil
                   (if (valid-anumber anumber)
                     "An email will soon be sent to ~a, please follow its instructions to verify your session."
                     "Invalid A-Number: ~a")
                   anumber)))
    (if (valid-anumber anumber)
      (send-token-to-aggie anumber))
    (setf (gethash :message (getf env :lack.session)) message)
    '(302 (:location "/"))))

(defun logout (env)
  (let ((session (getf env :lack.session)))
    (remhash :anumber session)
    (setf (gethash :message session) "You have been logged out"))

  '(302 (:location "/")))

(defun set-session-from-token (env)
  (let* ((params (getf env :query-parameters))
         (token (cdr (assoc "token" params :test #'string=)))
         (signature (gethash token *aggie-auth-tokens*))
         (session (getf env :lack.session))
         (maybe-anumber (valid-token-p-get-anumber signature)))
    (cond
      (maybe-anumber
        (setf (gethash :anumber session) maybe-anumber
              (gethash :message session) (format nil "You have authenticated yourself as ~a" maybe-anumber))
        '(302 (:location "/print")))
      (t
        '(401 (:content-type "text/plain")
          ("Invalid or expired token"))))))

;; Print

(defun copy-binary-stream-to-file (stream path)
  (with-open-file (filestream path
                              :direction :output
                              :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte stream nil)
          while byte
          do (write-byte byte filestream))))

(defun add-print-job (env)
  (case (getf env :request-method)
    (:get
      (list 200 '(:content-type "text/html")
            (list (job-form env))))
    (:post
      (let* ((params (getf env :body-parameters))
             (session (getf env :lack.session))
             (anumber (gethash :anumber session))

             (file-stream (cadr (assoc "payload" params :test #'string=)))
             (color (cdr (assoc "color" params :test #'string=)))
             (copies (cdr (assoc "copies" params :test #'string=)))
             (title (cdr (assoc "title" params :test #'string=)))

             (path (make-unique-pdf))
             (printer-name (make-unique-printer-name))
             (printer-uri (make-printer-uri anumber
                                            :color color))
             (create-printer-cmd (make-cups-create-printer-cmd printer-name printer-uri))
             (options-alist (mapcar
                              (lambda (option)
                                (cons
                                  option
                                  (cdr (assoc (string-downcase (string option))
                                              params :test #'string=))))
                              (mapcar #'car *CUPS-OPTIONS*)))
             (print-cmd (make-cups-print-command
                          printer-name
                          path
                          :title title
                          :copies copies
                          :options-alist options-alist))
             (remove-printer-cmd (make-cups-delete-printer-command printer-name)))

        (cond
          ((some (lambda (arg)
                   (null (cl-ppcre:all-matches *SAFE-ARG-REGEX* arg)))
                 (append (list color copies title)
                         (mapcar #'cdr options-alist)))
            (setf (gethash :message session)
                  (format nil "Seems like you tried something a little naughty..." title)))
          (t
            (copy-binary-stream-to-file
              (flexi-streams:make-flexi-stream file-stream
                                               :external-format :utf-8)
              path)
            (when (and
                    (typep file-stream 'file-stream)
                    (probe-file file-stream))
              (delete-file file-stream))

            (uiop:run-program create-printer-cmd)
            (uiop:run-program print-cmd)
            ;;(uiop:run-program remove-print-cmd)

            (setf (gethash :message session)
                  (format nil "Print job for \"~a\" was sent!" title))))

        '(302 (:location "/print"))))))

(defun home (env)
  (if (is-authenticated env)
    '(302 (:location "/print"))
    (list 200 '(:content-type "text/html")
          (list (login-page env)))))

(defun fallback (env)
  (list 404 '(:content-type "text/html")
        (list (four-oh-four env))))

(defun health-check (env)
  (list 200 '(:content-type "text/plain")
        (list "healthy")))

;; Simple routing

(defvar *routes*
  '(("^/health" . health-check)
    ("^/print" . add-print-job)
    ("^/token" . request-token)
    ("^/auth" . set-session-from-token)
    ("^/logout" . logout)
    ("^/$" . home)
    ("" . fallback)))

(defvar *protected-route-regexs* '("^/print" "^/logout"))

(defun is-authenticated (env)
  (not (null (gethash :anumber
                      (getf env :lack.session)))))

(defun route-req (env)
  (let* ((uri (getf env :request-uri))
         (route (find-if (lambda (route)
                           (cl-ppcre:all-matches (car route) uri))
                         *routes*))
         (route-regex (car route))
         (route-handler (cdr route))
         (route-protected (some (lambda (regex)
                                  (cl-ppcre:all-matches regex uri))
                                *protected-route-regexs*)))
    (if route-protected
      (if (is-authenticated env)
        (funcall route-handler env)
        (list 401 '(:content-type "text/plain")
              (list "Unauthorized")))
      (funcall route-handler env))))

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
        'route-req))

(defun start ()
  (clack:clackup *app*
                 :port 4000
                 :address "0.0.0.0"))

(defun stop ()
  (clack:stop *app*))

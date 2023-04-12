(asdf:defsystem "usuprintcl"
  :version "0.0.1"
  :author "Simponic"
  :license "MIT"
  :depends-on (:clack
               :lack
               :alexandria
               :lack-middleware-session
               :cl-ppcre
               :clack-handler-hunchentoot
               :jose
               :drakma
               :cl-markup
               :cl-json)
  :components ((:file "app")))

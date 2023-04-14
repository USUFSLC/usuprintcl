(asdf:defsystem "usuprintcl"
  :version "0.0.1"
  :author "Simponic"
  :license "MIT"
  :depends-on (:clack
               :lack
               :lack-middleware-session
               :lack-session-store-redis
               :cl-ppcre
               :clack-handler-hunchentoot
               :jose
               :drakma
               :cl-markup
               :cl-json)
  :components ((:file "app")))

(load "usuprintcl.asd")
(ql:quickload :usuprintcl)

(usuprintcl.app:start)
(bt:join-thread
 (find-if (lambda (thread)
            (let ((name (bt:thread-name thread)))
              (when (or (search "hunchentoot" name)
                        (search "cl-cron" name)) ;; Can be removed since no longer needed
                thread)))
          (bt:all-threads)))

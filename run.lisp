(load "usuprintcl.asd")
(ql:quickload :usuprintcl)

(usuprintcl.app:start)
(bt:join-thread
 (find-if (lambda (th)
            (search "hunchentoot"
                    (bt:thread-name th)))
          (bt:all-threads)))

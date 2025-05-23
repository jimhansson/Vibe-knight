(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :asdf)
(push (truename ".") asdf:*central-registry*)
#+asdf (pushnew (truename "./src") asdf:*central-registry*)

;; Explicitly load PROVE testing framework
(ql:quickload :prove)

(ql:quickload :vibe-knight)


  ;; Explicitly stop verbose threads
  (ql:quickload :verbose)
  (org.shirakumo.verbose:remove-global-controller)

  ;; Simplified thread management
  (handler-case
      (progn
        (when (fboundp 'sb-thread:list-all-threads)
          (dolist (thread (sb-thread:list-all-threads))
            (when (and (not (eq thread sb-thread:*current-thread*))
                       (not (string= (sb-thread:thread-name thread) "main thread")))
              (sb-thread:terminate-thread thread)))))
    (error () nil))

(sb-ext:save-lisp-and-die "vibe-knight"
  :toplevel #'vibe-knight:main
  :executable t)

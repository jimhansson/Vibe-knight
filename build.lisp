(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :asdf)
(push (truename ".") asdf:*central-registry*)
#+asdf (pushnew (truename "./src") asdf:*central-registry*)
(ql:quickload :vibe-knight)


  ;; setup verbose in a state that will allow use to save the image.
  (ql:quickload :verbose)
  (org.shirakumo.verbose:remove-global-controller)

;; these 2 lines are needed for some reason because else the verbose thread will still be running.
  (sb-thread:list-all-threads)
  (mapcar #'sb-thread:thread-name (sb-thread:list-all-threads))

(sb-ext:save-lisp-and-die "vibe-knight"
  :toplevel #'vibe-knight:main
  :executable t)

;;;; build.lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :asdf)
(asdf:load-system :vibe-knight)

(sb-ext:save-lisp-and-die "vibe-knight"
  :toplevel #'vibe-knight:main
  :executable t)

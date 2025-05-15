;;;; build.lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :asdf)
(push (truename ".") asdf:*central-registry*)
(ql:quickload :vibe-knight)

(sb-ext:save-lisp-and-die "vibe-knight"
  :toplevel #'vibe-knight:main
  :executable t)

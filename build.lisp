;;;; build.lisp

(asdf:load-system :vibe-knight)

(sb-ext:save-lisp-and-die "vibe-knight"
  :toplevel #'vibe-knight:main
  :executable t)

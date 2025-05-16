;;;; package.lisp
(defpackage :vibe-knight
  (:use :cl :sdl2)
  (:export :main
           :make-initial-grid
           :neighbors-4
           :neighbors-8))

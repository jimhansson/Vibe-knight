;;;; package.lisp
(defpackage :vibe-knight
  (:use :cl)
  (:export :main
           :make-initial-grid
           :neighbors-4
           :neighbors-8
           :wave-function-collapse
           :get-cell
           :cell-collapsed-biome))

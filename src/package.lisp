;;;; package.lisp
(defpackage :vibe-knight
  (:use :cl :sdl2)
  (:local-nicknames (:v :org.shirakumo.verbose))
  (:export :main
           :make-initial-grid
           :neighbors-4
           :neighbors-8
           :wave-function-collapse
           :get-cell
           :cell-collapsed-biome))

(defpackage :vibe-knight/world-test
  (:use :cl :prove :vibe-knight))

(in-package :vibe-knight)

(defstruct player
  x
  y
  hp)

(defun move-player (player direction)
  ;; Stub for player movement logic
  (format t "Moving player ~A~%" direction))

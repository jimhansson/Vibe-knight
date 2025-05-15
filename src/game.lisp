(in-package :vibe-knight)

(defun start-game ()
  (generate-map)
  (let ((player (make-player :x 1 :y 1 :hp 10)))
    (format t "Game started!~%")))

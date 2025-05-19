(in-package :vibe-knight)

(defstruct player
  x
  y
  hp
  (scene-x (floor (/ *scenes-x* 2)) :type fixnum)
  (scene-y (floor (/ *scenes-y* 2)) :type fixnum)
  (tile-x (floor (/ *scene-tiles-x* 2)) :type fixnum)
  (tile-y (floor (/ *scene-tiles-y* 2)) :type fixnum))

(defun move-player (player direction)
  ;; Stub for player movement logic
  (format t "Moving player ~A~%" direction))

(defun move-player-to-tile (player new-tile-x new-tile-y)
  "Move player within the current scene, or to a new scene if out of bounds."
  (let ((scene-x (player-scene-x player))
        (scene-y (player-scene-y player)))
    (cond
      ((< new-tile-x 0)
       (setf (player-scene-x player) (max 0 (1- scene-x))
             (player-tile-x player) (1- *scene-tiles-x*)))
      ((>= new-tile-x *scene-tiles-x*)
       (setf (player-scene-x player) (min (1- *scenes-x*) (1+ scene-x))
             (player-tile-x player) 0))
      ((< new-tile-y 0)
       (setf (player-scene-y player) (max 0 (1- scene-y))
             (player-tile-y player) (1- *scene-tiles-y*)))
      ((>= new-tile-y *scene-tiles-y*)
       (setf (player-scene-y player) (min (1- *scenes-y*) (1+ scene-y))
             (player-tile-y player) 0))
      (t
       (setf (player-tile-x player) new-tile-x
             (player-tile-y player) new-tile-y)))))

(defun make-player (&key (x 0) (y 0) (hp 100))
  "Create a player starting in the middle of the world grid."
  (let* ((scene-x (floor (/ *scenes-x* 2)))
         (scene-y (floor (/ *scenes-y* 2)))
         (tile-x (floor (/ *scene-tiles-x* 2)))
         (tile-y (floor (/ *scene-tiles-y* 2))))
    (make-player :x x :y y :hp hp
                 :scene-x scene-x :scene-y scene-y
                 :tile-x tile-x :tile-y tile-y)))

(in-package :vibe-knight)

;; Tile, Scene, and World structures for a 16x16 tile-based world

(defparameter *tile-width* 16)
(defparameter *tile-height* 16)
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *tiles-per-row* (/ *screen-width* *tile-width*)) ; 40
(defparameter *tiles-per-col* (/ *screen-height* *tile-height*)) ; 30
(defparameter *scene-tiles-x* (* *tiles-per-row* 5)) ; 200
(defparameter *scene-tiles-y* (* *tiles-per-col* 5)) ; 150
(defparameter *scenes-x* 6)
(defparameter *scenes-y* 6)

(defstruct scene
  "A scene is a 5x5 screen area of tiles."
  (tiles (make-array (list *scene-tiles-x* *scene-tiles-y*)) :type (simple-array t 2))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct world
  "A world is a grid of scenes."
  (scenes (make-array (list *scenes-x* *scenes-y*)) :type (simple-array scene 2)))

;; Player navigation logic moved to player.lisp for separation of concerns.

(defun get-current-scene (world player)
  (aref (world-scenes world) (player-scene-x player) (player-scene-y player)))

(defun ensure-scene (world x y &optional (generator #'make-scene))
  "Ensure a scene exists at (x, y), generating it if needed."
  (let ((scene (aref (world-scenes world) x y)))
    (unless scene
      (setf (aref (world-scenes world) x y) (funcall generator :x x :y y)))
    (aref (world-scenes world) x y)))

(defun render-scene (scene draw-tile-fn)
  "Render all tiles in the scene using draw-tile-fn, which takes (tile x y)."
  (dotimes (x *scene-tiles-x*)
    (dotimes (y *scene-tiles-y*)
      (let ((tile (aref (scene-tiles scene) x y)))
        (funcall draw-tile-fn tile x y)))))

;; Example draw-tile-fn (replace with your graphics code)
;; (defun draw-tile (tile x y)
;;   (let ((px (* x *tile-width*))
;;         (py (* y *tile-height*)))
;;     ;; (sdl2:draw-image tile px py)
;;     ))

;; Example usage:
;; (defparameter *my-world* (make-world))
;; (defparameter *player* (make-player))
;; (defparameter *current-scene* (ensure-scene *my-world* (player-scene-x *player*) (player-scene-y *player*)))
;; (render-scene *current-scene* #'draw-tile)
;; (move-player-to-tile *player* (1+ (player-tile-x *player*)) (player-tile-y *player*))
(in-package :vibe-knight)

;; Structures for a world composed of multiple scenes, where each tile is 16x16 pixels

(defparameter *tile-width* 16)
(defparameter *tile-height* 16)
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *tiles-per-row* (/ *screen-width* *tile-width*)) ; 40
(defparameter *tiles-per-col* (/ *screen-height* *tile-height*)) ; 30
(defparameter *scene-tiles-x* (* *tiles-per-row* 5)) ; 200
(defparameter *scene-tiles-y* (* *tiles-per-col* 5)) ; 150
(defparameter *scenes-x* 7)
(defparameter *scenes-y* 7)

(defstruct scene
  "A scene is a 5x5 screen area of tiles."
  (tiles (make-array (list *scene-tiles-x* *scene-tiles-y*) :element-type t))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct world
  "A world is a grid of scenes. Each scene represents a 5x5 screen area of tiles, 
   and the world is composed of a grid of these scenes. The `scenes` field is a 
   2D array where each element is a `scene` struct, allowing the world to be 
   navigated and rendered as a collection of interconnected scenes."
  (scenes (make-array (list *scenes-x* *scenes-y*) :element-type 'scene)))

;; Player navigation logic moved to player.lisp for separation of concerns.

(defun get-current-scene (world player)
  (let* ((x (player-scene-x player))
         (y (player-scene-y player))
         (max-x (1- (array-dimension (world-scenes world) 0)))
         (max-y (1- (array-dimension (world-scenes world) 1))))
    (assert (<= 0 x max-x) (x) "Player scene-x index ~A is out of bounds (0..~A)." x max-x)
    (assert (<= 0 y max-y) (y) "Player scene-y index ~A is out of bounds (0..~A)." y max-y)
    (aref (world-scenes world) x y)))

(defun ensure-scene (world x y &optional (generator #'make-scene))
  "Ensure a scene exists at (x, y), generating it if needed."
  (let ((scene (aref (world-scenes world) x y)))
    (unless scene
      (setf (aref (world-scenes world) x y) (funcall generator :x x :y y)))
    (aref (world-scenes world) x y)))

(defun render-scene (scene draw-tile-fn)
  "Render all tiles in the scene using draw-tile-fn, which takes (tile x y).
   The draw-tile-fn function should return nil and handle any errors internally 
   to ensure rendering continues without interruption."
  (dotimes (x *scene-tiles-x*)
    (dotimes (y *scene-tiles-y*)
      (let ((tile (aref (scene-tiles scene) x y)))
        (when tile)))))
        
;; Example draw-tile-fn (this is a placeholder; replace with your actual graphics rendering code)
;; (defun draw-tile (tile x y)
;;   (let ((px (* x *tile-width*))
;;         (py (* y *tile-height*)))
;;     ;; Replace the following line with actual rendering logic, e.g., using a graphics library.
;;     ;; (sdl2:draw-image tile px py)
;;     ))
;;     ;; (sdl2:draw-image tile px py)
;;     ))

;; Example usage:
;; (defparameter *my-world* (make-world))
;; (defparameter *player* (make-player))
;; (defparameter *current-scene* (ensure-scene *my-world* (player-scene-x *player*) (player-scene-y *player*)))
;; (render-scene *current-scene* #'draw-tile)
;; (move-player-to-tile *player* (1+ (player-tile-x *player*)) (player-tile-y *player*))
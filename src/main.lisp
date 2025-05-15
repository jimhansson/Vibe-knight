(defpackage :vibe-knight
  (:use :cl :sdl2))
(in-package :vibe-knight)

(defun main ()
  (sdl2:init :everything)
  (let ((window (sdl2:create-window "vibe-knight" :w 640 :h 480 :flags '(:shown)))
        (renderer (sdl2:create-renderer window -1 0))
        (gamepad (when (> (sdl2:joystick-count) 0)
                   (sdl2:game-controller-open 0))))
    (unwind-protect
         (sdl2:with-event-loop ()
           (:quit () (setf gamepad (progn (when gamepad (sdl2:game-controller-close gamepad)) nil)))
           (:keydown (&rest args)
             (format t "Keydown args: ~S~%" args))
           (:controllerbuttondown (&rest args)
             (format t "Controllerbuttondown args: ~S~%" args))
           (:idle ()
             (sdl2:set-render-draw-color renderer 0 0 0 255)
             (sdl2:render-clear renderer)
             ;; Game rendering goes here
             (sdl2:render-present renderer)
             (sdl2:delay 16)))
      (progn
        (when gamepad (sdl2:game-controller-close gamepad))
        (sdl2:destroy-renderer renderer)
        (sdl2:destroy-window window)
        (sdl2:quit)))))

(defpackage :vibe-knight
  (:use :cl :sdl2))
(in-package :vibe-knight)

(defun main ()
  (sdl2:init :everything)
  (let ((window (sdl2:create-window "vibe-knight" :w 640 :h 480 :flags '(:shown)))
        (renderer (sdl2:create-renderer window -1 0)))
    (unwind-protect
         (event-loop window renderer)
      (sdl2:destroy-renderer renderer)
      (sdl2:destroy-window window)
      (sdl2:quit))))

(defun event-loop (window renderer)
  (let ((running t)
        (event (sdl2:make-event))
        (gamepad nil))
    ;; Försök öppna första gamepad
    (when (> (sdl2:num-joysticks) 0)
      (setf gamepad (sdl2:game-controller-open 0)))
    (unwind-protect
         (loop while running do
           (loop while (sdl2:poll-event event)
                 do (cond
                      ((sdl2:event-type= event :quit)
                       (setf running nil))
                      ((sdl2:event-type= event :controllerbuttondown)
                       (let ((button (sdl2:controller-button event)))
                         (format t "Gamepad button pressed: ~A~%" button)
                         ;; Exempel: avsluta på "start"
                         (when (eql button :start)
                           (setf running nil))))
                      ((sdl2:event-type= event :keydown)
                       (let ((key (sdl2:scancode->keyword (sdl2:keyboard-scancode event))))
                         (format t "Key pressed: ~A~%" key)
                         (when (eql key :escape)
                           (setf running nil))))))
           (sdl2:set-render-draw-color renderer 0 0 0 255)
           (sdl2:render-clear renderer)
           ;; Här kan du rita spelet
           (sdl2:render-present renderer)
           (sdl2:delay 16))
      (when gamepad (sdl2:game-controller-close gamepad)))))

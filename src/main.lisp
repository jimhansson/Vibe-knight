(defpackage :vibe-knight
  (:use :cl :sdl2))
(in-package :vibe-knight)

(defparameter *window* nil)
(defparameter *renderer* nil)

;; Handle Ctrl+C (SIGINT) to quit gracefully (SBCL-specific)
#+sbcl
(handler-bind ((sb-sys:interactive-interrupt
                (lambda (c)
                  (declare (ignore c))
                  (sdl2:push-quit-event))))
  (defun main ()
    (sdl2:init :everything)
    (setf *window* (sdl2:create-window :title "vibe-knight" :w 640 :h 480 :flags '(:shown)))
    (setf *renderer* (sdl2:create-renderer *window* -1 '(:software)))
    (sdl2:set-render-draw-color *renderer* 0 0 0 255)
    (sdl2:render-clear *renderer*)
    (sdl2:render-present *renderer*)
    (let ((gamepad (when (> (sdl2:joystick-count) 0)
                     (sdl2:game-controller-open 0))))
      (unwind-protect
           (sdl2:with-event-loop ()
             (:quit () (setf gamepad (progn (when gamepad (sdl2:game-controller-close gamepad)) nil)))
             (:keydown (&key sym)
               (when (or (string= sym "q") (string= sym "Q"))
                 (sdl2:push-quit-event))
               (format t "Keydown: ~S~%" sym))
             (:controllerbuttondown (&rest args)
               (format t "Controllerbuttondown args: ~S~%" args))
             (:idle ()
               (sdl2:set-render-draw-color *renderer* 0 0 0 255)
               (sdl2:render-clear *renderer*)
               ;; Game rendering goes here
               (sdl2:render-present *renderer*)
               (sdl2:delay 16)))
        (progn
          (when gamepad (sdl2:game-controller-close gamepad))
          (sdl2:destroy-renderer *renderer*)
          (sdl2:destroy-window *window*)
          (sdl2:quit))))))

;; For other implementations, just define main without handler-bind
#-sbcl
(defun main ()
  (sdl2:init :everything)
  (setf *window* (sdl2:create-window :title "vibe-knight" :w 640 :h 480 :flags '(:shown)))
  (setf *renderer* (sdl2:create-renderer *window* -1 '(:software)))
  (sdl2:set-render-draw-color *renderer* 0 0 0 255)
  (sdl2:render-clear *renderer*)
  (sdl2:render-present *renderer*)
  (let ((gamepad (when (> (sdl2:joystick-count) 0)
                   (sdl2:game-controller-open 0))))
    (unwind-protect
         (sdl2:with-event-loop ()
           (:quit () (setf gamepad (progn (when gamepad (sdl2:game-controller-close gamepad)) nil)))
           (:keydown (&key sym)
             (when (or (string= sym "q") (string= sym "Q"))
               (sdl2:push-quit-event))
             (format t "Keydown: ~S~%" sym))
           (:controllerbuttondown (&rest args)
             (format t "Controllerbuttondown args: ~S~%" args))
           (:idle ()
             (sdl2:set-render-draw-color *renderer* 0 0 0 255)
             (sdl2:render-clear *renderer*)
             ;; Game rendering goes here
             (sdl2:render-present *renderer*)
             (sdl2:delay 16)))
      (progn
        (when gamepad (sdl2:game-controller-close gamepad))
        (sdl2:destroy-renderer *renderer*)
        (sdl2:destroy-window *window*)
        (sdl2:quit)))))

(in-package :vibe-knight)

(defvar *gamepad* nil
  "The gamepad object, if any.")

(defun handle-game-initialization-error (condition exit-fn)
  "Shared error handling function for game initialization."
  (v:error :main "Initialization failed: ~A" condition)
  (format *error-output* "Game initialization failed: ~A~%" condition)
  (funcall exit-fn :code 1))

(defun setup-game ()
  "Initialize SDL2 and set up the gamepad if available."
  (v:info :main "Initializing SDL2")
  (init-sdl2)
  (v:info :main "SDL2 Initialized successfully")
  (setf *gamepad* (when (> (sdl2:joystick-count) 0)
                    (v:info :main "Gamepad detected, opening controller")
                    (sdl2:game-controller-open 0))))

(defun run-game-event-loop ()
  "Shared event loop function for both SBCL and non-SBCL implementations."
  (unwind-protect
       (sdl2:with-event-loop ()
         (:quit ()
           (v:debug :main "Quit event received")
           (when *gamepad* (sdl2:game-controller-close *gamepad*))
           (setf *gamepad* nil))
         (:keydown (:keysym sym)
           (v:debug :main "Keydown: ~S" sym)
           (when (or (string= sym "q") (string= sym "Q"))
             (sdl2:push-quit-event)))
         (:controllerbuttondown (&rest args)
           (v:debug :main "Controllerbuttondown args: ~S" args))
         (:idle ()
           (draw)
           (sdl2:delay 50)))
    (progn
      (v:info :main "Cleaning up resources")
      (when *gamepad* (sdl2:game-controller-close *gamepad*))
      (cleanup-sdl2)
      (v:info :main "Shutdown complete"))))

#+sbcl
(handler-bind ((sb-sys:interactive-interrupt
                 (lambda (c)
                   (declare (ignore c))
                   (sdl2:push-quit-event))))
  (defun main ()
    (v:restart-global-controller)
    (handler-case
        (progn
          (setup-game)
          (run-game-event-loop))
      (error (condition)
        (handle-game-initialization-error condition #'sb-ext:exit)))))

#-sbcl
(defun main ()
  (v:restart-global-controller)
  (handler-case
      (progn
        (setup-game)
        (run-game-event-loop))
    (error (condition)
      (handle-game-initialization-error condition #'exit))))

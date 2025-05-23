(in-package :vibe-knight)

(defparameter *window* nil)
(defparameter *renderer* nil)

(defun init-sdl2 ()
  "Initialize SDL2 with robust error handling and fallback mechanisms"
  (handler-case
      (progn
        (v:info :sdl "Attempting SDL2 initialization")
        
        ;; Comprehensive video driver attempts
        (let ((video-drivers '("dummy" "x11" "wayland" "windows" "cocoa" "directfb" "rpi"))
              (init-flags '(:everything :video :events)))
          
          ;; Try multiple video drivers and initialization strategies
          (block driver-loop
            (dolist (driver video-drivers)
              (setf (uiop:getenv "SDL_VIDEODRIVER") driver)
              (v:info :sdl "Trying video driver: ~A" driver)
              
                (when 
                        (let ((result (sdl2:init :everything)))
                          (v:info :sdl "Initialization with result: ~A" result)
                          result)
                  
                  ;; Attempt window creation with various strategies
                  (setf *window*
                        (or
                         (ignore-errors
                           (sdl2:create-window
                            :title "vibe-knight"
                            :w 640 :h 480
                            :flags '(:shown :borderless)))
                         (ignore-errors
                           (sdl2:create-window
                            :title "vibe-knight"
                            :w 640 :h 480
                            :flags '(:hidden)))
                         (ignore-errors
                           (sdl2:create-window
                            :title "vibe-knight"
                            :w 1 :h 1
                            :flags '(:hidden)))))
                  
                  (when *window*
                    (v:info :sdl "Window created successfully")
                    
                    ;; Attempt renderer creation with multiple strategies
                    (setf *renderer*
                          (or
                           (ignore-errors
                             (sdl2:create-renderer *window* -1 '(:software)))
                           (ignore-errors
                             (sdl2:create-renderer *window* -1 '(:accelerated)))
                           (ignore-errors
                             (sdl2:create-renderer *window* -1 nil))))
                    
                    (when *renderer*
                      (v:info :sdl "Renderer created successfully")
                      (sdl2:set-render-draw-color *renderer* 0 0 0 255)
                      (sdl2:render-clear *renderer*)
                      (sdl2:render-present *renderer*)
                      (return-from driver-loop t)))))
            
            ;; If no initialization works, log warning
            (v:warn :sdl "No successful SDL2 initialization found"))
          
          ;; Minimal initialization fallback
          (sdl2:init :events)
          t))
    
    (error (c)
      (v:error :sdl "SDL2 Initialization failed: ~A" c)
      (format *error-output* "SDL2 Initialization failed: ~A~%" c)
      nil)))

(defun draw ()
  "Draw a moving red rectangle with robust error handling"
  (handler-case
      (when *renderer*
        (let* ((time (mod (get-internal-real-time) 1000))
               (x (truncate (* 600 (/ time 1000.0))))
               (y 200)
               (w 40)
               (h 40))
          (sdl2:set-render-draw-color *renderer* 0 0 0 255)
          (sdl2:render-clear *renderer*)
          (sdl2:set-render-draw-color *renderer* 255 0 0 255)
          (sdl2:render-fill-rect *renderer* (sdl2:make-rect x y w h))
          (sdl2:render-present *renderer*)))
    (error (c)
      (v:error :sdl "Drawing error: ~A" c)
      nil)))

;; Removed duplicate draw function

(defun cleanup-sdl2 ()
  "Clean up SDL2 resources"
  (when *renderer*
    (sdl2:destroy-renderer *renderer*)
    (setf *renderer* nil))
  (when *window*
    (sdl2:destroy-window *window*)
    (setf *window* nil))
  (sdl2:quit))

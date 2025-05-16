(in-package :vibe-knight)

(defclass objective ()
  ((name :initarg :name 
         :accessor objective-name 
         :documentation "The name or id of the objective.")
   (requirements :initarg :requirements 
                 :accessor objective-requirements 
                 :initform nil 
                 :documentation "List of other objectives that must be completed first.")
   (status :initarg :status 
           :accessor objective-status 
           :initform :pending 
           :documentation "Status of the objective, e.g., :pending, :complete.")))

(defun make-objective (name &key (requirements nil) (status :pending))
  "Create a new objective object."
  (make-instance 'objective :name name :requirements requirements :status status))

(defun test-objectives ()
  "Return a list of three test objectives: a lock (requires key), a key, and a shop (independent)."
  (let* ((key (make-objective 'key))
         (lock (make-objective 'lock :requirements (list key)))
         (shop (make-objective 'shop)))
    (list lock key shop)))


;; Example usage:
;; (test-objectives)
;; (randomize-objectives-graph (test-objectives))



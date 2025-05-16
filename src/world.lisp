(in-package :vibe-knight)

;;;; Wave Function Collapse for Biomes
;; This is a minimal, extensible framework for generating a biome map using a wave function collapse algorithm.

(defparameter *biomes* '(:forest :desert :mountain :plains :swamp :lake))

(defstruct cell
  possible-biomes
  collapsed-biome)

(defun get-cell (grid x y)
  "Get the cell at (x, y) from the grid."
  (aref grid x y))

(defun set-cell (grid x y cell)
  "Set the cell at (x, y) in the grid."
  (setf (aref grid x y) cell))

(defun make-initial-grid (width height)
  "Create a grid of cells, each with all biomes possible."
  (let ((grid (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref grid x y)
              (make-cell :possible-biomes (copy-list *biomes*)
                         :collapsed-biome nil))))
    grid))

(defun neighbors-4 (grid x y)
  "Return the coordinates of the 4-neighbors of cell (x, y) in the grid."
  (let ((w (array-dimension grid 0))
        (h (array-dimension grid 1)))
    (remove-if-not #'identity
      (list (when (> x 0) (list (1- x) y))
            (when (< (1+ x) w) (list (1+ x) y))
            (when (> y 0) (list x (1- y)))
            (when (< (1+ y) h) (list x (1+ y)))))))

(defun neighbors-8 (grid x y)
  "Return the coordinates of the 8-neighbors of cell (x, y) in the grid."
  (let ((w (array-dimension grid 0))
        (h (array-dimension grid 1)))
    (remove-if-not #'identity
      (list
        (when (and (> x 0) (> y 0)) (list (1- x) (1- y)))         ; upper left
        (when (> y 0) (list x (1- y)))                           ; up
        (when (< (1+ x) w) (> y 0) (list (1+ x) (1- y)))         ; upper right
        (when (> x 0) (list (1- x) y))                           ; left
        (when (< (1+ x) w) (list (1+ x) y))                      ; right
        (when (> x 0) (< (1+ y) h) (list (1- x) (1+ y)))         ; lower left
        (when (< (1+ y) h) (list x (1+ y)))                      ; down
        (when (< (1+ x) w) (< (1+ y) h) (list (1+ x) (1+ y)))    ; lower right
      ))))

(defun weighted-random-choice (items weights)
  "Choose a random item from ITEMS according to WEIGHTS (alist of item . weight)."
  (let* ((total (reduce #'+ (mapcar #'cdr weights)))
         (r (random total))
         (sum 0))
    (dolist (item items)
      (let ((w (or (cdr (assoc item weights)) 1)))
        (incf sum w)
        (when (>= sum r)
          (return item))))))

(defun collapse-cell (cell &optional (weights nil))
  "Collapse a cell by choosing a random biome from its possible biomes, using weights if provided."
  (let* ((choices (cell-possible-biomes cell))
         (choice (if weights
                     (weighted-random-choice choices weights)
                     (nth (random (length choices)) choices))))
    (setf (cell-collapsed-biome cell) choice
          (cell-possible-biomes cell) (list choice))
    choice))

(defun propagate (grid x y &key (neighbor-fn #'neighbors-4))
  "Propagate constraints from the collapsed cell at (x, y) to its neighbors."
  (let* ((cell (get-cell grid x y))
         (biome (cell-collapsed-biome cell)))
    (dolist (coord (funcall neighbor-fn grid x y))
      (destructuring-bind (nx ny) coord
        (let ((neighbor (get-cell grid nx ny)))
          (unless (cell-collapsed-biome neighbor)
            (setf (cell-possible-biomes neighbor)
                  (remove biome (cell-possible-biomes neighbor)))))))))

(defun uncollapsed-cells (grid width height)
  "Return a list of (x y) coordinates for all uncollapsed cells in the grid."
  (loop for x below width append
        (loop for y below height
              unless (cell-collapsed-biome (get-cell grid x y))
              collect (list x y))))

(defun wave-function-collapse (width height &key (weights nil) (neighbor-fn #'neighbors-4))
  "Generate a biome map using a simple wave function collapse algorithm. Optionally provide a weights alist and neighbor function."
  (let ((grid (make-initial-grid width height)))
    (loop repeat (* width height)
          do (let ((uncollapsed (uncollapsed-cells grid width height)))
               (when (null uncollapsed) (return))
               (destructuring-bind (x y) (nth (random (length uncollapsed)) uncollapsed)
                 (collapse-cell (get-cell grid x y) weights)
                 (propagate grid x y :neighbor-fn neighbor-fn))))
    grid))

;; Example usage:
;; (wave-function-collapse 10 10 :weights '((:forest . 5) (:desert . 1) (:mountain . 1) (:plains . 2) (:swamp . 1) (:lake . 1)))
;; (wave-function-collapse 10 10 :neighbor-fn #'neighbors-8)

(in-package :vibe-knight)

;;;; General Wave Function Collapse Framework
;; This is a minimal, extensible framework for generating a constraint-satisfying grid using a wave function collapse algorithm.

(defstruct cell
  possible-values  ; List of values this cell could still become (before collapse)
  collapsed-value) ; The value this cell has been collapsed to, or NIL if not yet collapsed

(defun make-initial-grid (width height values)
  "Create a grid of cells, each with all possible values."
  (let ((grid (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref grid x y)
              (make-cell :possible-values (copy-list values)
                         :collapsed-value nil))))
    grid))

(defun get-cell (grid x y)
  "Get the cell at (x, y) from the grid."
  (aref grid x y))

(defun set-cell (grid x y cell)
  "Set the cell at (x, y) in the grid."
  (setf (aref grid x y) cell))

;; Neighbor functions for general grid adjacency
(defun neighbors-4 (grid x y)
  "Return a list of (nx ny) for the 4 orthogonal neighbors of (x, y) within grid bounds."
  (let* ((width (array-dimension grid 0))
         (height (array-dimension grid 1))
         (candidates (list (list (- x 1) y)
                           (list (+ x 1) y)
                           (list x (- y 1))
                           (list x (+ y 1)))))
    (remove-if-not (lambda (coord)
                     (let ((nx (first coord)) (ny (second coord)))
                       (and (>= nx 0) (< nx width)
                            (>= ny 0) (< ny height))))
                   candidates)))

(defun neighbors-8 (grid x y)
  "Return a list of (nx ny) for the 8 neighbors (including diagonals) of (x, y) within grid bounds."
  (let* ((width (array-dimension grid 0))
         (height (array-dimension grid 1))
         (candidates (loop for dx from -1 to 1 append
                           (loop for dy from -1 to 1
                                 unless (and (= dx 0) (= dy 0))
                                 collect (list (+ x dx) (+ y dy))))))
    (remove-if-not (lambda (coord)
                     (let ((nx (first coord)) (ny (second coord)))
                       (and (>= nx 0) (< nx width)
                            (>= ny 0) (< ny height))))
                   candidates)))


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
  "Collapse a cell by choosing a random value from its possible values, using weights if provided."
  (let* ((choices (cell-possible-values cell))
         (choice (if weights
                     (weighted-random-choice choices weights)
                     (nth (random (length choices)) choices))))
    (setf (cell-collapsed-value cell) choice
          (cell-possible-values cell) (list choice))
    choice))

(defun propagate (grid x y &key (neighbor-fn #'neighbors-4) (remove-fn #'remove))
  "Propagate constraints from the collapsed cell at (x, y) to its neighbors. Optionally provide a custom remove-fn for constraint logic."
  (let* ((cell (get-cell grid x y))
         (value (cell-collapsed-value cell)))
    (dolist (coord (funcall neighbor-fn grid x y))
      (destructuring-bind (nx ny) coord
        (let ((neighbor (get-cell grid nx ny)))
          (unless (cell-collapsed-value neighbor)
            (setf (cell-possible-values neighbor)
                  (funcall remove-fn value (cell-possible-values neighbor)))))))))

(defun uncollapsed-cells (grid width height)
  "Return a list of (x y) coordinates for all uncollapsed cells in the grid."
  (loop for x below width append
        (loop for y below height
              unless (cell-collapsed-value (get-cell grid x y))
              collect (list x y))))

(defun min-entropy-candidates (grid coords)
  "Return the list of coords with the minimum entropy in the grid (optimized single pass)."
  (when coords
    (let ((min-entropy most-positive-fixnum)
          (candidates '()))
      (dolist (coord coords)
        (let ((entropy (length (cell-possible-values
                               (get-cell grid (first coord) (second coord))))))
          (cond ((< entropy min-entropy)
                 (setf min-entropy entropy
                       candidates (list coord)))
                ((= entropy min-entropy)
                 (push coord candidates)))))
      candidates)))

(defun copy-grid (grid)
  "Create a deep copy of the grid for backtracking."
  (let* ((width (array-dimension grid 0))
         (height (array-dimension grid 1))
         (new-grid (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (let ((cell (get-cell grid x y)))
          (setf (aref new-grid x y)
                (make-cell :possible-values (copy-list (cell-possible-values cell))
                           :collapsed-value (cell-collapsed-value cell))))))
    new-grid))

(defun grid-complete-p (grid)
  "Check if all cells in the grid are collapsed."
  (let ((width (array-dimension grid 0))
        (height (array-dimension grid 1)))
    (loop for x below width do
      (loop for y below height do
        (unless (cell-collapsed-value (get-cell grid x y))
          (return-from grid-complete-p nil))))
    t))

;; Default collapse and propagate functions for user override
(defun default-collapse-cell (cell weights)
  (collapse-cell cell weights))

(defun default-propagate (grid x y &key neighbor-fn remove-fn)
  (propagate grid x y :neighbor-fn neighbor-fn :remove-fn remove-fn))

(defun wave-function-collapse (grid remove-fn &key weights
                                         (neighbor-fn #'neighbors-4)
                                         (collapse-fn #'default-collapse-cell)
                                         (propagate-fn #'default-propagate)
                                         (max-attempts 10)
                                         on-collapse on-propagate on-contradiction)
  "General wave function collapse algorithm with backtracking. The user must provide the grid and a mandatory remove-fn for constraint logic. At each step, pick the uncollapsed cell with the fewest possible values (lowest entropy), breaking ties randomly. Collapse and propagation can be customized. Hooks: on-collapse, on-propagate, on-contradiction. Includes backtracking for contradiction handling."
  (loop repeat max-attempts do
    (let ((working-grid (copy-grid grid))
          (width (array-dimension grid 0))
          (height (array-dimension grid 1))
          (success t))
      (loop repeat (* width height)
            do (let* ((uncollapsed (uncollapsed-cells working-grid width height)))
                 (when (null uncollapsed)
                   (return-from wave-function-collapse working-grid))
                 (let* ((candidates (min-entropy-candidates working-grid uncollapsed))
                        (chosen (nth (random (length candidates)) candidates))
                        (x (first chosen))
                        (y (second chosen))
                        (cell (get-cell working-grid x y)))
                   ;; Check for contradiction BEFORE collapse
                   (when (= (length (cell-possible-values cell)) 0)
                     (when on-contradiction
                       (funcall on-contradiction working-grid x y cell))
                     (setf success nil)
                     (return)) ; Break out of inner loop to retry
                   
                   (let ((collapsed-value (funcall collapse-fn cell weights)))
                     (when on-collapse (funcall on-collapse working-grid x y cell collapsed-value)))
                   
                   (funcall propagate-fn working-grid x y :neighbor-fn neighbor-fn :remove-fn remove-fn)
                   (when on-propagate (funcall on-propagate working-grid x y cell)))))
      (when success (return-from wave-function-collapse working-grid)))
    finally (error "Failed to generate valid grid after ~A attempts" max-attempts)))

;; Example usage:
;; (let ((grid (make-initial-grid 10 10 *biomes*)))
;;   (wave-function-collapse grid #'remove))
;; (let ((grid (make-initial-grid 10 10 '(:a :b :c :d))))
;;   (wave-function-collapse grid #'remove :weights '((:a . 5) (:b . 1) (:c . 1) (:d . 1))))
;; (let ((grid (make-initial-grid 10 10 '(0 1 2 3 4 5))))
;;   (wave-function-collapse grid #'remove :neighbor-fn #'neighbors-8))
;;
;; Example: Using a hash table of constraints for collapse
;; (let* ((values '(a b c d))
;;        (constraints (make-hash-table))
;;        (grid (make-initial-grid 5 5 values)))
;;   ;; Suppose 'a can only be next to 'b, 'b can be next to 'a or 'c, etc.
;;   (setf (gethash 'a constraints) '(b))
;;   (setf (gethash 'b constraints) '(a c))
;;   (setf (gethash 'c constraints) '(b d))
;;   (setf (gethash 'd constraints) '(c))
;;   (defun hash-remove-fn (value possible)
;;     (let ((allowed (gethash value constraints)))
;;       (remove-if-not (lambda (v) (member v allowed)) possible)))
;;   (wave-function-collapse grid #'hash-remove-fn))
;;
;; Example: Custom on-collapse hook to print each collapse
;; (defun my-on-collapse (grid x y cell value)
;;   (format t "Collapsed cell (~A,~A) to ~A~%" x y value))
;; (wave-function-collapse grid #'remove :on-collapse #'my-on-collapse)

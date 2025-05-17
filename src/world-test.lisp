(in-package :cl-user)
(defpackage :vibe-knight/world-test
  (:use :cl :prove :vibe-knight))
(in-package :vibe-knight/world-test)

;; Import WFC symbols for testing
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :vibe-knight)
    (error "Package :vibe-knight not found!"))
  (use-package :vibe-knight))

(plan 1)

(defun coord< (a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b)) (< (second a) (second b)))))

;; We use copy-list to avoid destructive modification of constant data by sort.
(subtest "neighbors"
  (subtest "neighbors-4 corners and center"
    (let ((grid (make-initial-grid 3 3 '(a b c))))
      (is (sort (neighbors-4 grid 1 1) #'coord<)
          (sort (copy-list '((0 1) (2 1) (1 0) (1 2))) #'coord<)
          "Center cell has 4 neighbors")
      (is (neighbors-4 grid 0 0)
          '((1 0) (0 1))
          "Top-left corner has 2 neighbors")
      (is (neighbors-4 grid 2 2)
          '((1 2) (2 1))
          "Bottom-right corner has 2 neighbors")))

  (subtest "neighbors-8 corners and center"
    (let ((grid (make-initial-grid 3 3 '(a b c))))
      (is (sort (neighbors-8 grid 1 1) #'coord<)
          (sort (copy-list '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2))) #'coord<)
          "Center cell has 8 neighbors")
      (is (sort (neighbors-8 grid 0 0) #'coord<)
          (sort (copy-list '((0 1) (1 0) (1 1))) #'coord<)
          "Top-left corner has 3 neighbors")
      (is (sort (neighbors-8 grid 2 2) #'coord<)
          (sort (copy-list '((1 1) (1 2) (2 1))) #'coord<)
          "Bottom-right corner has 3 neighbors"))))

  (subtest "wave-function-collapse"
    (let ((grid (make-initial-grid 3 3 '(a b c))))
      (wave-function-collapse grid #'remove)
      (ok grid "Returns a grid")
      (is (array-dimension grid 0) 3 "Grid width is 3")
      (is (array-dimension grid 1) 3 "Grid height is 3")
      (let ((all-collapsed t))
        (dotimes (x 3)
          (dotimes (y 3)
            (unless (cell-collapsed-value (get-cell grid x y))
              (setf all-collapsed nil))))
        (ok all-collapsed "All cells are collapsed")))
    (let* ((weights '((a . 10) (b . 1) (c . 1)))
           (grid (make-initial-grid 5 5 '(a b c))))
      (wave-function-collapse grid #'remove :weights weights)
      (let ((a-count 0))
        (dotimes (x 5)
          (dotimes (y 5)
            (when (eq (cell-collapsed-value (get-cell grid x y)) 'a)
              (incf a-count))))
        (ok (> a-count 0) "Weighted collapse produces at least one 'a' cell"))))

(finalize)

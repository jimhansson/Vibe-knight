(in-package :cl-user)
(defpackage :vibe-knight/world-test
  (:use :cl :prove :vibe-knight))
(in-package :vibe-knight/world-test)

(plan 1)

(defun coord< (a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b)) (< (second a) (second b)))))

;; We use copy-list to avoid destructive modification of constant data by sort.
(subtest "neighbors"
  (subtest "neighbors-4 corners and center"
    (let ((grid (make-initial-grid 3 3)))
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
    (let ((grid (make-initial-grid 3 3)))
      (is (sort (neighbors-8 grid 1 1) #'coord<)
          (sort (copy-list '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2))) #'coord<)
          "Center cell has 8 neighbors")
      (is (sort (neighbors-8 grid 0 0) #'coord<)
          (sort (copy-list '((0 1) (1 0) (1 1))) #'coord<)
          "Top-left corner has 3 neighbors")
      (is (sort (neighbors-8 grid 2 2) #'coord<)
          (sort (copy-list '((1 1) (1 2) (2 1))) #'coord<)
          "Bottom-right corner has 3 neighbors"))))

(finalize)

(in-package :cl-user)
(defpackage :vibe-knight/world-test
  (:use :cl :prove :vibe-knight))
(in-package :vibe-knight/world-test)

(plan 4)

(defun coord< (a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b)) (< (second a) (second b)))))

;; Test neighbors-4
(subtest "neighbors-4 at center"
  (let ((grid (make-initial-grid 3 3)))
    (is (sort (neighbors-4 grid 1 1) #'coord<)
        (sort '((0 1) (2 1) (1 0) (1 2)) #'coord<)
        "Center cell has 4 neighbors")))

(subtest "neighbors-4 at corner"
  (let ((grid (make-initial-grid 3 3)))
    (is (neighbors-4 grid 0 0)
        '((1 0) (0 1))
        "Top-left corner has 2 neighbors")))

;; Test neighbors-8
(subtest "neighbors-8 at center"
  (let ((grid (make-initial-grid 3 3)))
    (is (sort (neighbors-8 grid 1 1) #'coord<)
        (sort '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)) #'coord<)
        "Center cell has 8 neighbors")))

(subtest "neighbors-8 at edge"
  (let ((grid (make-initial-grid 3 3)))
    (is (sort (neighbors-8 grid 0 1) #'coord<)
        (sort '((0 0) (0 2) (1 0) (1 1) (1 2)) #'coord<)
        "Edge cell has correct neighbors")))

(finalize)

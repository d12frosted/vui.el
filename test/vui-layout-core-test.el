;;; vui-layout-core-test.el --- Pure layout core tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the pure layout core in vui-layout.el (issue #134):
;; proportional shares, row partitioning at minimum widths, per-row
;; allocation with grow and shrink, end-to-end placements, and block
;; composition.  Everything here is pure - no buffers, no vnodes, no
;; measurement - so the specs double as the core's semantics
;; documentation (and as conformance cases for the shared-seam
;; exploration in yibie/textui#1).
;;
;; Not to be confused with vui-layout-test.el, which tests vui.el's
;; buffer-level layout primitives (vstack, hstack, box, list, table).

;;; Code:

(require 'buttercup)
(require 'vui-layout)

;;; Proportional shares

(describe "vui-layout-shares"
  (it "splits an amount proportionally to weights"
    (expect (vui-layout-shares 30 '(1 2)) :to-equal '(10 20)))

  (it "hands the integer remainder out from the first eligible weight"
    ;; floors are 3/3/3, remainder 1 goes to the first weight
    (expect (vui-layout-shares 10 '(1 1 1)) :to-equal '(4 3 3)))

  (it "distributes a multi-unit remainder round-robin in source order"
    ;; floors are 3/3/3, remainder 2 goes to the first two weights
    (expect (vui-layout-shares 11 '(1 1 1)) :to-equal '(4 4 3)))

  (it "gives zero-weight entries nothing"
    (expect (vui-layout-shares 10 '(0 1 0)) :to-equal '(0 10 0)))

  (it "returns all zeros when the total weight is zero"
    (expect (vui-layout-shares 10 '(0 0)) :to-equal '(0 0)))

  (it "caps each share at its limit and redistributes the rest"
    (expect (vui-layout-shares 10 '(1 1) '(2 100)) :to-equal '(2 8)))

  (it "stops when every limit is exhausted"
    ;; only 3 cells of capacity exist; 7 remain unassigned
    (expect (vui-layout-shares 10 '(1 1) '(1 2)) :to-equal '(1 2)))

  (it "handles an empty weight list"
    (expect (vui-layout-shares 10 nil) :to-equal nil)))

;;; Partitioning into rows

(describe "vui-layout-partition"
  (it "keeps children on one row while their minima fit"
    (expect (vui-layout-partition
             (list '(:natural 4) '(:natural 4)) 10 1)
            :to-equal '((0 1))))

  (it "wraps in source order when minima plus gaps stop fitting"
    (expect (vui-layout-partition
             (list '(:natural 4) '(:natural 4) '(:natural 4)) 10 1)
            :to-equal '((0 1) (2))))

  (it "partitions at minimum widths, not natural widths"
    ;; naturals 8+8 overflow 10, but minima 4+4+gap fit
    (expect (vui-layout-partition
             (list '(:natural 8 :min 4) '(:natural 8 :min 4)) 10 1)
            :to-equal '((0 1))))

  (it "never emits an empty row, even for an oversized first child"
    (expect (vui-layout-partition (list '(:natural 20)) 10 1)
            :to-equal '((0))))

  (it "accounts for the gap between children"
    ;; 5 + gap 2 + 5 = 12 > 10: wraps
    (expect (vui-layout-partition
             (list '(:natural 5) '(:natural 5)) 10 2)
            :to-equal '((0) (1))))

  (it "returns no rows for no children"
    (expect (vui-layout-partition nil 10 1) :to-equal nil)))

;;; Per-row allocation

(describe "vui-layout-allocate"
  (it "gives every child its natural width when there is no surplus"
    (expect (vui-layout-allocate
             (list '(:natural 4) '(:natural 5)) 10 1)
            :to-equal '(4 5)))

  (it "distributes surplus to growers proportionally"
    (expect (vui-layout-allocate
             (list '(:natural 2 :grow 1) '(:natural 2 :grow 1)) 11 1)
            :to-equal '(5 5)))

  (it "leaves non-growers at natural width under surplus"
    ;; available 10, naturals 4: the lone grower absorbs all 6 extra
    (expect (vui-layout-allocate
             (list '(:natural 2) '(:natural 2 :grow 1)) 11 1)
            :to-equal '(2 8)))

  (it "shrinks proportionally to capacity under deficit, never below min"
    ;; available 10, naturals 16: deficit 6 split by capacities 4 and 2
    (expect (vui-layout-allocate
             (list '(:natural 8 :min 4) '(:natural 8 :min 6)) 11 1)
            :to-equal '(4 6)))

  (it "does not shrink rigid children below natural"
    (expect (vui-layout-allocate
             (list '(:natural 8 :min 4) '(:natural 6 :rigid t)) 13 1)
            :to-equal '(6 6)))

  (it "floors at minima when the deficit exceeds capacity"
    ;; available 10, naturals 16, capacity 4: the row overflows at minima
    (expect (vui-layout-allocate
             (list '(:natural 8 :min 6) '(:natural 8 :min 6)) 11 1)
            :to-equal '(6 6)))

  (it "clamps a lone shrinkable child to the available width"
    (expect (vui-layout-allocate (list '(:natural 20)) 10 1)
            :to-equal '(10)))

  (it "keeps a lone rigid child at natural width even when it overflows"
    (expect (vui-layout-allocate (list '(:natural 20 :rigid t)) 10 1)
            :to-equal '(20))))

;;; End-to-end placements

(describe "vui-layout-solve"
  (it "returns one placement per child in source order"
    (let ((placements (vui-layout-solve
                       (list '(:natural 4 :grow 1) '(:natural 4)
                             '(:natural 8))
                       10 1)))
      (expect (length placements) :to-equal 3)
      ;; row 0: children 0 and 1 (4+1+4 fits in 10), grower takes surplus
      (expect (nth 0 placements) :to-equal '(:row 0 :column 0 :width 5))
      (expect (nth 1 placements) :to-equal '(:row 0 :column 1 :width 4))
      ;; row 1: child 2 alone
      (expect (nth 2 placements) :to-equal '(:row 1 :column 0 :width 8))))

  (it "reproduces the wide/narrow responsive behavior"
    ;; the A B / C D case: four children, wide shows 2x2, narrow 1x4
    (let ((specs (list '(:natural 3) '(:natural 3)
                       '(:natural 3) '(:natural 3))))
      (expect (mapcar (lambda (p) (plist-get p :row))
                      (vui-layout-solve specs 7 1))
              :to-equal '(0 0 1 1))
      (expect (mapcar (lambda (p) (plist-get p :row))
                      (vui-layout-solve specs 3 1))
              :to-equal '(0 1 2 3))))

  (it "solves no children to no placements"
    (expect (vui-layout-solve nil 10 1) :to-equal nil)))

;;; Block composition

(describe "vui-layout-compose"
  (it "joins blocks side by side, padded to their widths"
    (expect (vui-layout-compose
             (list '("ab") '("cdef")) '(3 4) 1)
            :to-equal '("ab  cdef")))

  (it "pads shorter blocks with blank lines to the tallest block"
    (expect (vui-layout-compose
             (list '("a" "b") '("x")) '(1 1) 1)
            :to-equal '("a x" "b  ")))

  (it "preserves text properties of block lines"
    (let* ((line (propertize "ab" 'face 'bold))
           (composed (vui-layout-compose (list (list line)) '(4) 1)))
      (expect (get-text-property 0 'face (car composed)) :to-equal 'bold)))

  (it "widens a column when a block overflows its assigned width"
    ;; block 0 renders wider than its assignment: the whole column
    ;; widens deterministically so later lines stay aligned
    (expect (vui-layout-compose
             (list '("toolong" "a") '("x" "y")) '(3 1) 1)
            :to-equal '("toolong x" "a       y")))

  (it "measures and pads through the supplied functions"
    ;; a fake unit system where every char is 2 units wide and padding
    ;; is dots: proves the composer itself never measures
    (expect (vui-layout-compose
             (list '("ab") '("c")) '(8 2) 2
             (lambda (line) (* 2 (length line)))
             (lambda (amount) (make-string (/ amount 2) ?.)))
            :to-equal '("ab...c")))

  (it "composes an empty block list to no lines"
    (expect (vui-layout-compose nil nil 1) :to-equal nil)))

;;; Grid

(describe "vui-layout-grid-columns"
  (it "caps the column count at the requested columns"
    (expect (vui-layout-grid-columns 4 5 100 1) :to-equal 4))

  (it "drops columns when minimum column widths stop fitting"
    ;; two 10-wide columns plus one gap fit in 25; three do not
    (expect (vui-layout-grid-columns 4 10 25 1) :to-equal 2))

  (it "counts the gaps between columns, not after the last one"
    ;; 10 + 1 + 10 = 21 exactly fits two columns
    (expect (vui-layout-grid-columns 4 10 21 1) :to-equal 2)
    (expect (vui-layout-grid-columns 4 10 20 1) :to-equal 1))

  (it "never returns fewer than one column"
    (expect (vui-layout-grid-columns 4 10 3 1) :to-equal 1))

  (it "uses the requested columns when no minimum is given"
    (expect (vui-layout-grid-columns 3 nil 10 1) :to-equal 3)))

(describe "vui-layout-grid-tracks"
  (it "splits the width net of gaps into equal tracks"
    (expect (vui-layout-grid-tracks 3 20 1) :to-equal '(6 6 6)))

  (it "gives earlier tracks the uneven remainder"
    (expect (vui-layout-grid-tracks 3 21 1) :to-equal '(7 6 6)))

  (it "clamps to zero-width tracks when the gaps alone overflow"
    (expect (vui-layout-grid-tracks 3 1 2) :to-equal '(0 0 0))))

(describe "vui-layout-grid"
  (it "places children row-major with their track widths"
    (expect (vui-layout-grid 5 21 1 2)
            :to-equal '((:row 0 :column 0 :width 10)
                        (:row 0 :column 1 :width 10)
                        (:row 1 :column 0 :width 10)
                        (:row 1 :column 1 :width 10)
                        (:row 2 :column 0 :width 10))))

  (it "assigns uneven track widths by column"
    (expect (vui-layout-grid 3 22 1 3)
            :to-equal '((:row 0 :column 0 :width 7)
                        (:row 0 :column 1 :width 7)
                        (:row 0 :column 2 :width 6))))

  (it "collapses to one column below the minimum column width"
    (expect (vui-layout-grid 3 12 1 4 10)
            :to-equal '((:row 0 :column 0 :width 12)
                        (:row 1 :column 0 :width 12)
                        (:row 2 :column 0 :width 12))))

  (it "places no children as no placements"
    (expect (vui-layout-grid 0 20 1 2) :to-equal nil)))

(provide 'vui-layout-core-test)

;;; vui-layout-core-test.el ends here

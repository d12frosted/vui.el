;;; vui-layout.el --- Pure layout core: shares, wrap, placements -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; The pure layout core behind vui's responsive containers (issue #134).
;; Everything in this file is a pure function over integers, plists and
;; lists of strings: no buffers, no vnodes, no measurement, no vui
;; runtime.  That boundary is deliberate - it is what makes the core
;; testable in isolation and reusable outside vui (see the shared-seam
;; exploration in https://github.com/yibie/textui/issues/1).
;;
;; Vocabulary:
;;
;; - A SPEC describes one child's width constraints, as a plist:
;;   :natural N  measured natural width (required, non-negative)
;;   :min M      floor the child may shrink to (default: natural,
;;               i.e. not shrinkable); a floor above the natural
;;               width raises the width the child occupies, like
;;               CSS min-width
;;   :grow G     weight for distributing surplus width (default 0)
;;   :rigid t    never shrink below natural, even alone on an
;;               overflowing row (single-line atomic content such as
;;               buttons and fields)
;;
;; - A PLACEMENT is the result for one child: (:row R :column C
;;   :width W), in source order.  The core returns geometry; the
;;   consumer renders its own children into it.  Composing strings from
;;   placements is `vui-layout-compose'.
;;
;; - A BLOCK is a rendered rectangle: a list of lines (strings).
;;
;; Widths are unit-agnostic integers: measure content in cells and the
;; result is a cell layout, measure in pixels and it is a pixel layout.
;; The core never measures; even `vui-layout-compose' measures and pads
;; through caller-supplied functions (cells by default).
;;
;; Semantics (matching, where sensible, the engine in yibie/textui so
;; conformance comparison stays meaningful):
;;
;; - Partition: children fill a row while they fit at their minimum
;;   widths, in source order; the next child starts a new row.  A row
;;   is never empty, so an oversized first child still gets a row.
;; - Allocation: when naturals fit, surplus goes to growers
;;   proportionally to weights; when they do not, the deficit shrinks
;;   children proportionally to their capacity (natural minus min),
;;   never below min.  A lone child whose minimum exceeds the row is
;;   clamped to the available width unless it is rigid.
;; - Composition: a block wider than its assignment widens its column
;;   for the whole row, deterministically, instead of breaking
;;   alignment (the assignment is a target; overflow never truncates).
;; - Grid: equal integer tracks with the remainder on the earlier
;;   tracks; the column count falls with the available width, never
;;   below one; children fill rows in source order, so the final row
;;   may be incomplete.
;; - Height is content-driven: a row is as tall as its tallest block.

;;; Code:

(require 'cl-lib)

;;; Spec accessors

(defun vui-layout--natural (spec)
  "Return SPEC's effective natural width.
An explicit :min above the measured :natural raises it: the minimum
is a floor on the width the child occupies, as CSS min-width
overrides width."
  (max (or (plist-get spec :natural) 0)
       (or (plist-get spec :min) 0)))

(defun vui-layout--min (spec)
  "Return SPEC's effective minimum width.
Defaults to the natural width (not shrinkable); :rigid forces the
natural width."
  (if (plist-get spec :rigid)
      (vui-layout--natural spec)
    (or (plist-get spec :min) (vui-layout--natural spec))))

(defun vui-layout--grow (spec)
  "Return SPEC's grow weight."
  (or (plist-get spec :grow) 0))

;;; Proportional shares

(defun vui-layout-shares (amount weights &optional limits)
  "Split integer AMOUNT proportionally to WEIGHTS.
Return a list of non-negative integer shares, one per weight, summing
to at most AMOUNT.  Fractional remainders are handed out one unit at a
time starting from the first weight that is positive and still under
its limit.  Optional LIMITS caps each share; when every positive
weight is at its limit, the rest of AMOUNT stays unassigned."
  (let ((total (float (apply #'+ 0 weights)))
        (shares (make-list (length weights) 0))
        (remaining amount))
    ;; Both passes walk the lists with cons cursors: index-based
    ;; access would make each pass quadratic in the row's child count,
    ;; and this runs on every re-render of every shrinking row.
    (when (> total 0)
      (let ((ws weights) (ls limits) (ss shares))
        (while ws
          (let* ((raw (floor (* amount (/ (float (car ws)) total))))
                 (limit (car ls))
                 (share (if limit (min raw limit) raw)))
            (setcar ss share)
            (setq remaining (- remaining share)
                  ws (cdr ws) ls (cdr ls) ss (cdr ss)))))
      (while (> remaining 0)
        (let ((ws weights) (ls limits) (ss shares)
              progressed)
          (while (and ss (> remaining 0))
            (when (and (> (car ws) 0)
                       (or (null (car ls)) (< (car ss) (car ls))))
              (setcar ss (1+ (car ss)))
              (setq remaining (1- remaining)
                    progressed t))
            (setq ws (cdr ws) ls (cdr ls) ss (cdr ss)))
          (unless progressed
            (setq remaining 0)))))
    shares))

;;; Partitioning

(defun vui-layout-partition (specs total gap)
  "Partition SPECS into rows fitting TOTAL at minimum widths with GAP.
Return a list of rows, each a list of child indices, in source order.
A child joins the current row while the row's minima plus gaps fit
TOTAL; otherwise it starts a new row.  A row is never empty."
  (let ((index 0)
        (current nil)
        (current-width 0)
        (rows nil))
    (dolist (spec specs)
      (let* ((minimum (vui-layout--min spec))
             (joined (+ current-width (if current gap 0) minimum)))
        (if (or (null current) (<= joined total))
            (setq current (cons index current)
                  current-width joined)
          (push (nreverse current) rows)
          (setq current (list index)
                current-width minimum)))
      (setq index (1+ index)))
    (when current
      (push (nreverse current) rows))
    (nreverse rows)))

;;; Per-row allocation

(defun vui-layout-allocate (row-specs total gap)
  "Allocate widths for ROW-SPECS inside TOTAL separated by GAP.
Return a list of widths, one per spec.  Surplus goes to growers
proportionally to their weights; a deficit shrinks children
proportionally to capacity (natural minus min), never below min.  A
lone child whose minimum exceeds the available width is clamped to it
unless it is :rigid."
  (when row-specs
    (let* ((count (length row-specs))
           (available (max 0 (- total (* gap (max 0 (1- count))))))
           (naturals (mapcar #'vui-layout--natural row-specs))
           (minimums (mapcar #'vui-layout--min row-specs))
           (natural-total (apply #'+ 0 naturals)))
      (cond
       ;; Lone child too wide even at its minimum: clamp, unless rigid.
       ((and (= count 1) (> (car minimums) available))
        (if (plist-get (car row-specs) :rigid)
            (list (car naturals))
          (list available)))
       ;; Surplus (or exact fit): grow into the extra.
       ((<= natural-total available)
        (let* ((extra (- available natural-total))
               (weights (mapcar #'vui-layout--grow row-specs))
               (shares (vui-layout-shares extra weights)))
          (cl-mapcar #'+ naturals shares)))
       ;; Deficit: shrink proportionally to capacity, respecting floors.
       (t
        (let* ((overflow (- natural-total available))
               (capacities (cl-mapcar #'- naturals minimums))
               (capacity (apply #'+ 0 capacities))
               (reductions (vui-layout-shares
                            (min overflow capacity) capacities capacities)))
          (cl-mapcar #'- naturals reductions)))))))

;;; End-to-end placements

(defun vui-layout-solve (specs total gap)
  "Lay SPECS out inside TOTAL with GAP; return placements.
Partition into rows at minimum widths, allocate each row, and return
one placement plist (:row R :column C :width W) per child, in source
order.  The column is the child's position within its row."
  ;; Rows are contiguous in source order, so one cursor over SPECS
  ;; replaces per-row index lookups (which would be quadratic in the
  ;; child count) and placements build front-to-back.
  (let ((rest specs)
        (placements nil)
        (row-index 0))
    (dolist (row (vui-layout-partition specs total gap))
      (let* ((count (length row))
             (widths (vui-layout-allocate (seq-take rest count) total gap))
             (column 0))
        (dolist (width widths)
          (push (list :row row-index :column column :width width)
                placements)
          (setq column (1+ column)))
        (setq rest (nthcdr count rest)
              row-index (1+ row-index))))
    (nreverse placements)))

;;; Grid

(defun vui-layout-grid-columns (columns min-width total gap)
  "Return the responsive column count for a grid inside TOTAL.
COLUMNS is the requested count; MIN-WIDTH, when non-nil, is the
smallest acceptable track, so the count falls until MIN-WIDTH tracks
separated by GAP fit TOTAL.  Never returns fewer than one column."
  (max 1
       (min columns
            (if min-width
                ;; A minimum and gap summing to zero (or less) cannot
                ;; constrain the count; guard the division.
                (/ (+ total gap) (max 1 (+ min-width gap)))
              columns))))

(defun vui-layout-grid-tracks (count total gap)
  "Split TOTAL net of GAP into COUNT equal integer track widths.
The integer remainder goes to the earlier tracks, one unit each."
  (let ((available (max 0 (- total (* gap (max 0 (1- count)))))))
    (vui-layout-shares available (make-list count 1))))

(defun vui-layout-grid (count total gap columns &optional min-width)
  "Place COUNT children on an equal-track grid inside TOTAL.
COLUMNS and MIN-WIDTH pick the column count as in
`vui-layout-grid-columns'; GAP separates tracks.  Children fill rows
in source order, so the final row may be incomplete.  Return one
placement plist (:row R :column C :width W) per child, where the
width is the child's track width."
  (let* ((column-count (vui-layout-grid-columns columns min-width total gap))
         (tracks (vui-layout-grid-tracks column-count total gap))
         (placements nil))
    (dotimes (index count)
      (let ((column (% index column-count)))
        (push (list :row (/ index column-count)
                    :column column
                    :width (nth column tracks))
              placements)))
    (nreverse placements)))

;;; Block composition

(defun vui-layout--pad-line (line measured width pad)
  "Pad LINE, whose width is MEASURED, out to WIDTH with PAD."
  (let ((missing (- width measured)))
    (if (> missing 0)
        (concat line (funcall pad missing))
      line)))

(defun vui-layout-compose (blocks widths gap &optional measure pad)
  "Compose BLOCKS side by side at WIDTHS separated by GAP.
BLOCKS is a list of blocks (each a list of strings); WIDTHS assigns
each block a width.  A block whose widest line exceeds its assignment
widens its column for the whole row, so lines stay aligned (overflow
never truncates).  Shorter blocks are padded with blank lines to the
tallest block's height.  MEASURE maps a line to its width and PAD maps
an amount to padding, in the same units as WIDTHS and GAP; they
default to `string-width' and a run of spaces (cell units).  Return
the composed lines."
  (when blocks
    (let* ((measure (or measure #'string-width))
           (pad (or pad (lambda (amount) (make-string amount ?\s))))
           (height (apply #'max (mapcar #'length blocks)))
           (separator (funcall pad gap))
           ;; Measure every line exactly once; the widths feed both
           ;; the column maxima and the per-line padding (measuring
           ;; again while padding would double the cost of the
           ;; composer's hot loop).
           (measured (mapcar (lambda (block)
                               (mapcar (lambda (line)
                                         (funcall measure line))
                                       block))
                             blocks))
           (columns (cl-mapcar (lambda (line-widths width)
                                 (apply #'max width line-widths))
                               measured widths))
           (lines nil))
      (dotimes (line-index height)
        (let (parts)
          (cl-mapc (lambda (block line-widths width)
                     (push (vui-layout--pad-line
                            (or (nth line-index block) "")
                            (or (nth line-index line-widths) 0)
                            width pad)
                           parts))
                   blocks measured columns)
          (push (mapconcat #'identity (nreverse parts) separator) lines)))
      (nreverse lines))))

(provide 'vui-layout)

;;; vui-layout.el ends here

;;; vui-bench.el --- Performance benchmarks for vui.el -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A characterization suite for vui.el's render performance. The goal is
;; not micro-numbers but finding where vui starts to hurt: how cost
;; scales with content, what a small update in a large UI costs, how a
;; streaming append into a growing buffer behaves, and how widget-heavy
;; UIs compare to plain text.
;;
;; Methodology: each measurement warms up (untimed), then times K runs
;; and reports the full distribution - MIN (least-noisy estimator for "is
;; A faster than B", it filters scheduler/GC hiccups), MEDIAN (typical
;; cost), MAX (worst sample), and mean GC time (allocation pressure). A
;; ratio is only trustworthy when the two [min,max] ranges do not
;; overlap; `vui-bench--compare' interleaves the variants round-robin (so
;; slow system drift biases neither) and flags overlap explicitly.
;;
;; Confidence also means measuring the path we think we are: the
;; comparison scenarios assert PARITY (every variant leaves identical
;; buffer text - a fast wrong answer is not a win) and MECHANISM (a
;; render counter proves the bailout actually skipped the children it
;; claims to, rather than silently falling back to a full rebuild).
;;
;; Several scenarios exist specifically to gate the incremental-rendering
;; work (issue #82): worst case (everything changed - pure overhead for a
;; diffing renderer), best case (nothing changed - dirty-check floor),
;; localized updates by position, and keyed reorder. Today they measure
;; the wholesale (erase+rebuild) baseline; once incremental rendering
;; lands behind a flag, run with and without it and compare.
;;
;; These are not run by `eldev test' (they are slow and noisy). Run:
;;
;;   eldev emacs --batch -l benchmarks/vui-bench.el -f vui-bench-run
;;
;; or interactively: load this file and M-x vui-bench-run.

;;; Code:

(require 'vui)
(require 'benchmark)
(require 'cl-lib)

;; The agent-chat seam example (a transcript growing above a persistent
;; box) drives the streaming-seam benchmarks (`vui-bench-agent-*' and the
;; `vui-stream' ones).  The Eldev file puts docs/examples on `load-path'.
(require '13-agent-chat)

;;; Harness

(defun vui-bench--ms (seconds)
  "Format SECONDS as milliseconds."
  (format "%.3f" (* 1000 seconds)))

(defun vui-bench--header (title)
  "Print a section TITLE."
  (message "")
  (message "=== %s ===" title))

(defun vui-bench--row (&rest cells)
  "Print a table row from CELLS (each a cons of (WIDTH . VALUE))."
  (message "%s"
           (mapconcat (lambda (c)
                        (let ((w (car c)) (s (format "%s" (cdr c))))
                          (concat s (make-string (max 0 (- w (length s))) ?\s))))
                      cells "  ")))

(cl-defstruct (vui-bench-stat (:constructor vui-bench-stat--create))
  "A timing distribution: all in seconds, N samples."
  min median max mean gc n)

(defun vui-bench--stats (times gcs)
  "Summarize TIMES (list of seconds) and GCS (list of gc seconds)."
  (let* ((sorted (sort (copy-sequence times) #'<))
         (n (length sorted)))
    (vui-bench-stat--create
     :min (car sorted)
     :max (car (last sorted))
     :median (nth (/ n 2) sorted)
     :mean (/ (apply #'+ sorted) (float n))
     :gc (/ (apply #'+ gcs) (float n))
     :n n)))

(defvar vui-bench-rounds 9
  "Default timed rounds per measurement (after warmup).
Odd so the median is a real sample.  Higher = steadier, slower.")

(defun vui-bench--measure (k thunk &optional warmups)
  "Warm up THUNK (WARMUPS times, default 2), then time it K times.
Return a `vui-bench-stat'.  A full GC runs before each sample (so the
sample is not charged for cleaning up the previous one); GC that fires
DURING a sample is reported via `vui-bench-stat-gc'."
  (dotimes (_ (or warmups 2)) (funcall thunk))
  (let (times gcs)
    (dotimes (_ k)
      (garbage-collect)
      (let ((r (benchmark-run 1 (funcall thunk))))
        (push (nth 0 r) times)
        (push (nth 2 r) gcs)))
    (vui-bench--stats times gcs)))

(defun vui-bench--ratio-note (a b)
  "Describe how many times faster stat A is than stat B (or nil).
Compares minima; appends \"(ranges overlap - not significant)\" when A's
slowest sample is not clearly separated from B's fastest, i.e. the speed
gap is within measurement noise."
  (when (and a b (> (vui-bench-stat-min a) 0))
    (let ((ratio (/ (vui-bench-stat-min b) (vui-bench-stat-min a))))
      (format "%.1fx%s" ratio
              (if (< (vui-bench-stat-min a) (vui-bench-stat-max b))
                  "" " (ranges overlap - not significant)")))))

(defun vui-bench--stat-cell (res)
  "Format stat RES as \"median (min..max) +gc\" in ms."
  (format "%s (%s..%s)%s"
          (vui-bench--ms (vui-bench-stat-median res))
          (vui-bench--ms (vui-bench-stat-min res))
          (vui-bench--ms (vui-bench-stat-max res))
          (if (> (vui-bench-stat-gc res) 1e-6)
              (concat " +" (vui-bench--ms (vui-bench-stat-gc res)) "gc")
            "")))

(defun vui-bench--result-row (label res)
  "Print LABEL and a measurement RES (a `vui-bench-stat')."
  (vui-bench--row (cons 9 label)
                  (cons 34 (concat (vui-bench--stat-cell res) " ms"))))

(defun vui-bench--assert (ok fmt &rest args)
  "Signal a loud error unless OK; FMT/ARGS describe the broken assumption.
A benchmark that does not measure what it claims is worse than no
benchmark, so a failed mechanism/parity check aborts the run."
  (unless ok
    (error "vui-bench assertion failed: %s" (apply #'format fmt args))))

(defvar vui-bench--render-count 0
  "Incremented by instrumented bench components on each render-fn call.
Lets a scenario prove which children actually re-rendered.")

(defun vui-bench--compare (rounds specs)
  "Round-robin time SPECS, returning an alist of (LABEL . `vui-bench-stat').
SPECS is a list of (LABEL THUNK &optional EXPECT).  Running every spec
once per round (for ROUNDS rounds) cancels slow system drift that would
otherwise bias whichever spec ran first.  When EXPECT (a string) is
given, the thunk must leave that exact buffer text or the run aborts -
this is the parity guard that keeps a faster-but-wrong variant honest."
  (dolist (s specs) (funcall (nth 1 s)) (funcall (nth 1 s))) ; warmup
  (let ((times (make-hash-table :test 'equal))
        (gcs (make-hash-table :test 'equal)))
    (dotimes (_ rounds)
      (dolist (s specs)
        (garbage-collect)
        (let ((r (benchmark-run 1 (funcall (nth 1 s)))))
          (when (nth 2 s)
            (vui-bench--assert (equal (buffer-string) (nth 2 s))
                               "%s parity: buffer != expected" (nth 0 s)))
          (push (nth 0 r) (gethash (nth 0 s) times))
          (push (nth 2 r) (gethash (nth 0 s) gcs)))))
    (mapcar (lambda (s)
              (cons (nth 0 s)
                    (vui-bench--stats (gethash (nth 0 s) times)
                                      (gethash (nth 0 s) gcs))))
            specs)))

;;; Components used by the scenarios

(vui-defcomponent vui-bench-text-list (n)
  :render (vui-list (number-sequence 1 (or n 0))
                    (lambda (i)
                      (vui-text (format "row %d - representative line of content here" i)))
                    #'identity))

(vui-defcomponent vui-bench-worst (n)
  ;; Every item's content changes whenever `gen' bumps, so a re-render
  ;; after bumping is an "everything changed" render.
  :state ((gen 0))
  :render (vui-list (number-sequence 1 (or n 0))
                    (lambda (i) (vui-text (format "row %d - gen %d" i gen)))
                    #'identity))

(vui-defcomponent vui-bench-keyed (items)
  ;; Keyed list driven by state; key is the car, label the cdr.
  :state ((data items))
  :render (vui-list data (lambda (it) (vui-text (cdr it))) #'car))

(vui-defcomponent vui-bench-button-list (n)
  :render (vui-list (number-sequence 1 (or n 0))
                    (lambda (i) (vui-button (format "btn %d" i) :on-click #'ignore))
                    #'identity))

(vui-defcomponent vui-bench-transcript ()
  :state ((lines nil))
  :render (vui-list (reverse lines) (lambda (l) (vui-text l)) #'identity))

(vui-defcomponent vui-bench-counter ()
  :state ((n 0))
  :render (vui-text (format "count: %d" n)))

(vui-defcomponent vui-bench-static (n)
  ;; should-update nil: every re-render reuses the cached vtree, so with
  ;; incremental rendering the whole-tree eq short-circuit applies.
  :should-update nil
  :state ((tick 0))
  :render (vui-list (number-sequence 1 (or n 0))
                    (lambda (i) (vui-text (format "row %d" i)))
                    #'identity))

(vui-defcomponent vui-bench-cell (id label)
  ;; opts into bailout: unchanged cells are skipped under incremental render
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-text (format "[%s:%s]" id label)))

(vui-defcomponent vui-bench-cell-list (items)
  :render (apply #'vui-vstack
                 (mapcar (lambda (it)
                           (vui-component 'vui-bench-cell
                             :key (car it) :id (car it) :label (cdr it)))
                         items)))

(defun vui-bench--items (n)
  "Return an alist of N (ID . LABEL) pairs."
  (cl-loop for i from 1 to n collect (cons i (format "row %d - content" i))))

(vui-defcomponent vui-bench-table-view (columns rows)
  :render (vui-table :columns columns :rows rows :border :unicode))

(defun vui-bench--table-columns (n)
  "Return N column specs, cycling through the three alignments.
Mixing alignments exercises all three padding branches rather than just
the left-aligned one."
  (cl-loop for i from 1 to n
           collect (list :header (format "col %d" i)
                         :align (nth (mod i 3) '(:left :right :center)))))

(defun vui-bench--table-rows (rows cols &optional vnode)
  "Return ROWS rows of COLS cells each.
With VNODE non-nil the cells are `vui-text' vnodes instead of plain
strings, which forces `vui--cell-visual-width' down its temp-buffer
render path instead of measuring the string directly."
  (cl-loop for r from 1 to rows
           collect (cl-loop for c from 1 to cols
                            for s = (format "r%dc%d value" r c)
                            collect (if vnode (vui-text s) s))))

;;; Scenarios

(defconst vui-bench--sizes '(50 200 500 1000 2000 4000)
  "Item counts swept by the scaling scenarios.")

(defun vui-bench-initial-render ()
  "Initial mount cost vs item count."
  (vui-bench--header "Initial render (mount N text rows)")
  (dolist (n vui-bench--sizes)
    (let ((buf (format "*vui-bench-init-%d*" n)))
      (vui-bench--result-row
       n (vui-bench--measure
          3 (lambda () (vui-mount (vui-component 'vui-bench-text-list :n n) buf))))
      (vui-unmount buf)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-rerender-unchanged ()
  "Best case: re-render with an unchanged tree (dirty-check floor)."
  (vui-bench--header "Re-render, unchanged tree (best case)")
  (dolist (n vui-bench--sizes)
    (let* ((buf (format "*vui-bench-rr-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-text-list :n n) buf)))
      (vui-bench--result-row
       n (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-rerender-all-changed ()
  "Worst case: re-render where every item's content changed.
A diffing renderer can only lose here (pure diff/marker overhead)."
  (vui-bench--header "Re-render, everything changed (worst case)")
  (dolist (n vui-bench--sizes)
    (let* ((buf (format "*vui-bench-wc-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-worst :n n) buf)))
      (vui-bench--result-row
       n (vui-bench--measure
          5 (lambda ()
              (let ((vui--current-instance inst))
                (vui-set-state :gen (1+ (plist-get (vui-instance-state inst) :gen)))))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-localized-update ()
  "Change ONE item (first/middle/last) in a list of N, by position."
  (vui-bench--header "Localized single-item update (N = 2000)")
  (let* ((n 2000)
         (base (vui-bench--items n)))
    (dolist (spec `(("first" . 0) ("middle" . ,(/ n 2)) ("last" . ,(1- n))))
      (let* ((pos (cdr spec))
             (buf (format "*vui-bench-loc-%s*" (car spec)))
             ;; alt differs from base only at POS (same key, new label)
             (alt (let ((c (copy-sequence base)))
                    (setf (nth pos c) (cons (car (nth pos base)) "row - CHANGED"))
                    c))
             (inst (vui-mount (vui-component 'vui-bench-keyed :items base) buf))
             (tog nil))
        (vui-bench--result-row
         (car spec)
         (vui-bench--measure
          7 (lambda ()
              (setq tog (not tog))
              (let ((vui--current-instance inst))
                (vui-set-state :data (if tog alt base))))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

(defun vui-bench-reorder ()
  "Keyed reorder (reverse) cost vs item count."
  (vui-bench--header "Keyed reorder (reverse) vs N")
  (dolist (n '(200 500 1000 2000))
    (let* ((buf (format "*vui-bench-ro-%d*" n))
           (base (vui-bench--items n))
           (rev (reverse base))
           (inst (vui-mount (vui-component 'vui-bench-keyed :items base) buf))
           (tog nil))
      (vui-bench--result-row
       n (vui-bench--measure
          5 (lambda ()
              (setq tog (not tog))
              (let ((vui--current-instance inst))
                (vui-set-state :data (if tog rev base))))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-streaming ()
  "Append lines into a growing transcript, one render per line."
  (vui-bench--header "Streaming append (per-line re-render as transcript grows)")
  (vui-bench--row '(10 . "at line") '(14 . "this append") '(14 . "cumulative"))
  (let* ((buf "*vui-bench-stream*")
         (inst (vui-mount (vui-component 'vui-bench-transcript) buf))
         (line "2026-06-24 12:00:00  log line with a bit of representative content")
         (samples '(100 500 1000 2000))
         (total 0.0))
    (dotimes (i 2000)
      (garbage-collect)
      (let ((el (car (benchmark-run 1
                       (let ((vui--current-instance inst))
                         (vui-set-state :lines (lambda (old) (cons line old))))))))
        (setq total (+ total el))
        (when (memq (1+ i) samples)
          (vui-bench--row (cons 10 (1+ i))
                          (cons 14 (concat (vui-bench--ms el) " ms"))
                          (cons 14 (concat (vui-bench--ms total) " ms"))))))
    (vui-unmount inst)
    (when (get-buffer buf) (kill-buffer buf))))

(defun vui-bench-throughput ()
  "Raw re-render throughput for a trivial UI (lower bound per render)."
  (vui-bench--header "Update throughput (trivial counter)")
  (let* ((buf "*vui-bench-tp*")
         (inst (vui-mount (vui-component 'vui-bench-counter) buf))
         (res (vui-bench--measure
               3 (lambda ()
                   (let ((vui--current-instance inst))
                     (dotimes (_ 2000) (vui-set-state :n #'1+)))))))
    (vui-unmount inst)
    (when (get-buffer buf) (kill-buffer buf))
    (vui-bench--row '(16 . "2000 updates")
                    (cons 14 (concat (vui-bench--ms (vui-bench-stat-min res)) " ms")))
    (vui-bench--row '(16 . "per update")
                    (cons 14 (concat (vui-bench--ms (/ (vui-bench-stat-min res) 2000.0)) " ms")))))

(defun vui-bench-skip-knob ()
  "Re-render cost when should-update returns nil (the manual skip knob)."
  (vui-bench--header "should-update=nil re-render (manual skip knob)")
  (dolist (n '(500 2000))
    (let* ((buf (format "*vui-bench-sk-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-static :n n) buf)))
      (vui-bench--result-row
       n (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-widgets ()
  "Re-render cost vs interactive widget count (buttons)."
  (vui-bench--header "Widgets (full re-render of N buttons)")
  (dolist (n '(50 200 500 1000))
    (let* ((buf (format "*vui-bench-w-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-button-list :n n) buf)))
      (vui-bench--result-row
       n (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

;;; Tables
;;
;; Tables are the measurement-heavy corner of vui: column widths are
;; recomputed from scratch on every render, so a cell is measured
;; several times per pass (once sizing the column, then again for the
;; content and the padding).  That makes them the scenario where the
;; cost of the width primitive itself shows up, which is what issue #121
;; (pixel-based alignment) turns on.

(defconst vui-bench--table-sizes '(50 200 500 1000 2000)
  "Row counts swept by the table scenarios.
Stops short of `vui-bench--sizes': a table row costs several string
measurements per column, so it gets expensive well before 4000.")

(defvar vui-bench--width-mode-ok (boundp 'vui-width-mode)
  "Non-nil when the loaded vui has the char/pixel width switch (issue #121).")

(defun vui-bench-table ()
  "Full table re-render cost vs row count (5 columns, unicode border).
Every render re-measures every cell to size the columns, so expect a
steeper line than the plain-text scaling scenarios - this is the shape
that decides whether a more expensive width primitive is affordable."
  (vui-bench--header "Table re-render (N rows x 5 columns, unicode border)")
  (let ((cols (vui-bench--table-columns 5)))
    (dolist (n vui-bench--table-sizes)
      (let* ((buf (format "*vui-bench-tbl-%d*" n))
             (rows (vui-bench--table-rows n 5))
             (inst (vui-mount (vui-component 'vui-bench-table-view
                                             :columns cols :rows rows)
                              buf)))
        ;; Mechanism: a table that silently rendered short would report a
        ;; flattering number, so prove the last row actually made it in.
        (with-current-buffer buf
          (vui-bench--assert
           (save-excursion
             (goto-char (point-min))
             (search-forward (format "r%dc5 value" n) nil t))
           "table N=%d: last row missing from the render" n))
        (vui-bench--result-row
         n (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

(defun vui-bench-table-columns ()
  "Table re-render cost vs COLUMN count at a fixed 500 rows.
Measurement work scales with cells, not rows, so this should track the
row sweep: doubling the columns costs about what doubling the rows does.
A steeper line than that means per-column work beyond the cells."
  (vui-bench--header "Table re-render (500 rows x N columns)")
  (dolist (c '(2 5 10 20))
    (let* ((buf (format "*vui-bench-tblc-%d*" c))
           (cols (vui-bench--table-columns c))
           (rows (vui-bench--table-rows 500 c))
           (inst (vui-mount (vui-component 'vui-bench-table-view
                                           :columns cols :rows rows)
                            buf)))
      (vui-bench--result-row
       (format "%d cols" c)
       (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf)))))

(defun vui-bench-table-cells ()
  "String cells vs vnode cells at the same size (500 rows x 5 columns).
`vui--cell-visual-width' measures a string directly, but renders a vnode
into a temp buffer to measure it.  The gap between these two rows is
what a component inside a table costs over plain text."
  (vui-bench--header "Table cell kind (500 rows x 5 columns)")
  (let ((cols (vui-bench--table-columns 5)))
    (dolist (spec '(("string" . nil) ("vnode" . t)))
      (let* ((buf (format "*vui-bench-tblk-%s*" (car spec)))
             (rows (vui-bench--table-rows 500 5 (cdr spec)))
             (inst (vui-mount (vui-component 'vui-bench-table-view
                                             :columns cols :rows rows)
                              buf)))
        (vui-bench--result-row
         (car spec)
         (vui-bench--measure 5 (lambda () (vui--rerender-instance inst))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

(defun vui-bench-table-width-mode ()
  "Table re-render cost under each width mode (issue #121).
Pixel alignment swaps `string-width' for `string-pixel-width', which is
far more expensive per call, so this is the gate on whether pixel mode
can be the default rather than opt-in.  Timed round-robin, so system
drift cannot bias one mode over the other.

Two caveats on reading it.  In batch the modes produce IDENTICAL buffer
text (a tty frame is one pixel per column) but NOT identical cost - the
pixel path still pays for `window-text-pixel-size'.  So the numbers here
are real while the visual difference is not, and the default should be
decided from a GUI frame.  Second, the ratio only means something within
one build: run it before and after caching work rather than comparing
against a number from another machine.

Does nothing on builds without the switch."
  (when vui-bench--width-mode-ok
    (vui-bench--header "Table width mode (1000 rows x 5 columns)")
    (let* ((cols (vui-bench--table-columns 5))
           (rows (vui-bench--table-rows 1000 5))
           (cells nil)
           (specs nil))
      (dolist (mode '(char pixel))
        (let* ((label (symbol-name mode))
               (buf (format "*vui-bench-tblw-%s*" label))
               (inst (cl-progv '(vui-width-mode) (list mode)
                       (vui-mount (vui-component 'vui-bench-table-view
                                                 :columns cols :rows rows)
                                  buf))))
          (push (list label inst buf mode) cells)))
      (setq cells (nreverse cells))
      ;; Each spec re-renders its own instance under its own mode, so the
      ;; round-robin never leaves a mode measuring the other's buffer.
      (dolist (cell cells)
        (let ((inst (nth 1 cell))
              (mode (nth 3 cell)))
          (push (list (nth 0 cell)
                      (lambda ()
                        (cl-progv '(vui-width-mode) (list mode)
                          (vui--rerender-instance inst))))
                specs)))
      (setq specs (nreverse specs))
      (let ((results (vui-bench--compare vui-bench-rounds specs)))
        (dolist (cell cells)
          (vui-bench--result-row (nth 0 cell)
                                 (cdr (assoc (nth 0 cell) results))))
        (vui-bench--row
         (cons 30 "pixel cost (pixel vs char)")
         (cons 30 (or (vui-bench--ratio-note (cdr (assoc "char" results))
                                             (cdr (assoc "pixel" results)))
                      "n/a"))))
      (dolist (cell cells)
        (vui-unmount (nth 1 cell))
        (when (get-buffer (nth 2 cell)) (kill-buffer (nth 2 cell)))))))

;;;###autoload
(defun vui-bench-component-bailout ()
  "Localized one-item change in a list of should-update component children.
Reports wholesale vs incremental so the component-list bailout's effect
on the per-instance commit floor is visible."
  (vui-bench--header "Component-list localized update (should-update children)")
  (dolist (n '(500 2000))
    (dolist (flag '((nil . "wholesale") (t . "incremental")))
      (let* ((vui-incremental-render (car flag))
             (items (vui-bench--items n))
             (mid (/ n 2))
             (alt (let ((c (copy-sequence items)))
                    (setf (nth mid c) (cons (car (nth mid items)) "CHANGED"))
                    c))
             (buf "*vui-bench-cb*")
             (inst (vui-mount (vui-component 'vui-bench-cell-list :items items) buf))
             (tog nil))
        (vui-update inst (list :items items))
        (vui-bench--result-row
         (format "%d %s" n (cdr flag))
         (vui-bench--measure
          7 (lambda ()
              (setq tog (not tog))
              (vui-update inst (list :items (if tog alt items))))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

(vui-defcomponent vui-bench-msg (id)
  :should-update nil   ; content fixed per id; a streamed message never changes
  :render (vui-text (format "[%d] log line content here" id)))

(vui-defcomponent vui-bench-msg-list (ids)
  :render (apply #'vui-vstack
                 (mapcar (lambda (id) (vui-component 'vui-bench-msg :key id :id id)) ids)))

(defun vui-bench-streaming-growth ()
  "Cost of appending ONE message as a component transcript grows.
This is the pi.el-style case (a log that grows without bound).  The
slope of cost vs size is what matters: a flat slope means O(1) append
\(supports unbounded growth); a rising slope means O(n) per append and
thus O(n^2) to stream N (does not).  Reported wholesale vs incremental."
  (vui-bench--header "Streaming growth: append 1 message at transcript size S")
  (dolist (s '(200 500 1000 2000 4000))
    (dolist (flag '((nil . "wholesale") (t . "incremental")))
      (let* ((vui-incremental-render (car flag))
             (base (number-sequence 1 s))
             (plus (append base (list (1+ s))))
             (buf "*vui-bench-sg*")
             (inst (vui-mount (vui-component 'vui-bench-msg-list :ids base) buf))
             (tog nil))
        (vui-update inst (list :ids base))
        (vui-bench--result-row
         (format "%d %s" s (cdr flag))
         (vui-bench--measure
          5 (lambda () (setq tog (not tog))
              (vui-update inst (list :ids (if tog plus base))))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

;;; Agent-chat streaming seam (issue #82, dir 5: vui-stream)
;;
;; The pi.el-shaped case WITH the hard part: a transcript that grows
;; above a persistent declarative box (status + input field).  Two costs
;; matter, both O(n) today and both targets for `vui-stream':
;;
;;   append-growth  (Path 1): cost of appending one message vs size S.
;;     The stream owns its region and should append in O(1) -> FLAT
;;     slope.  Today the transcript is a plain vstack rebuilt every
;;     render -> rising slope (O(n) per append, O(n^2) to stream N).
;;
;;   box-update     (Path 2): cost of a box-only state change (status)
;;     vs size S.  Once the stream node is opaque to reconcile this
;;     should be O(box), independent of S -> FLAT.  Today a box change
;;     re-renders the whole transcript too -> rising slope.
;;
;; These establish the "before" numbers.  When `vui-stream' lands, both
;; slopes must flatten while the buffer stays byte-identical to the
;; wholesale baseline (the oracle in test/vui-agent-chat-test.el).

(defun vui-bench--agent-messages (n)
  "Return N agent-chat messages cycling through the three roles."
  (cl-loop for i from 1 to n
           collect (list :id i
                         :role (nth (mod i 3) '(user agent tool))
                         :text (format "message %d content" i))))

(defun vui-bench--agent-teardown (inst buf)
  "Unmount INST and kill BUF, tolerating the field-widget unmount caveat."
  (let ((inhibit-modification-hooks t))
    (ignore-errors (vui-unmount inst)))
  (when (get-buffer buf)
    (let ((inhibit-modification-hooks t)) (kill-buffer buf))))

(defun vui-bench-agent-append-growth ()
  "Cost of appending ONE message as the agent-chat transcript grows.
The chat box (status + input field) sits BELOW the transcript, so this
measures the real seam, not a bare append-only log.  A flat slope means
O(1) append (the `vui-stream' target); a rising slope means O(n) per
append.  Reported wholesale vs incremental."
  (vui-bench--header "Agent-chat append: +1 msg at transcript size S (box below)")
  (dolist (s '(200 500 1000 2000 4000))
    (dolist (flag '((nil . "wholesale") (t . "incremental")))
      (let* ((vui-incremental-render (car flag))
             (base (vui-bench--agent-messages s))
             (plus (append base (list (list :id (1+ s) :role 'agent
                                            :text "freshly streamed line"))))
             (buf "*vui-bench-agent-sg*")
             (inst (vui-mount (vui-component 'vui-agent-chat
                               :messages base :queue 0 :status "idle")
                              buf))
             (tog nil))
        (vui-update inst (list :messages base :queue 0 :status "idle"))
        (vui-bench--result-row
         (format "%d %s" s (cdr flag))
         (vui-bench--measure
          5 (lambda () (setq tog (not tog))
              (vui-update inst (list :messages (if tog plus base)
                                     :queue 0 :status "idle")))))
        (vui-bench--agent-teardown inst buf)))))

(defun vui-bench-agent-box-update ()
  "Cost of a BOX-ONLY state change as the transcript grows.
Toggles the box status while the transcript stays fixed at size S.  The
box change costs O(box), but today it re-renders the whole transcript
too, so the slope rises; with `vui-stream' the transcript is opaque to
this re-render and the slope should go flat.  Wholesale vs incremental."
  (vui-bench--header "Agent-chat box update: toggle status at transcript size S")
  (dolist (s '(200 500 1000 2000 4000))
    (dolist (flag '((nil . "wholesale") (t . "incremental")))
      (let* ((vui-incremental-render (car flag))
             (msgs (vui-bench--agent-messages s))
             (buf "*vui-bench-agent-box*")
             (inst (vui-mount (vui-component 'vui-agent-chat
                               :messages msgs :queue 0 :status "idle")
                              buf))
             (tog nil))
        (vui-update inst (list :messages msgs :queue 0 :status "idle"))
        (vui-bench--result-row
         (format "%d %s" s (cdr flag))
         (vui-bench--measure
          5 (lambda () (setq tog (not tog))
              (vui-update inst (list :messages msgs :queue (if tog 1 0)
                                     :status (if tog "busy" "idle"))))))
        (vui-bench--agent-teardown inst buf)))))

;; The imperative counterpart: a `vui-stream' above a box line.  Appends
;; go through `vui-stream-append', which owns the region and writes one
;; spot in O(1) - so this slope should be FLAT where the declarative
;; agent-append-growth above rises.
(vui-defcomponent vui-bench-stream-app (stream)
  :render (vui-vstack (vui-stream stream)
                      (vui-text "---- input box ----")))

(defun vui-bench-stream-append-growth ()
  "Cost of ONE `vui-stream-append' as the stream grows, box below.
The pass/fail metric for vui-stream (#82): a FLAT slope means O(1)
append, independent of stream size - contrast the rising slope of
`vui-bench-agent-append-growth', which rebuilds the transcript."
  (vui-bench--header "Stream append: +1 item at stream size S (box below)")
  (dolist (s '(200 500 1000 2000 4000))
    (let* ((handle (vui-make-stream))
           (buf "*vui-bench-stream*")
           (inst (vui-mount (vui-component 'vui-bench-stream-app :stream handle) buf)))
      ;; pre-fill (the first item pays a one-time re-render, the rest O(1))
      (dotimes (i s)
        (vui-stream-append handle (vui-text (format "message %d content" i))))
      (vui-bench--result-row
       (format "%d stream" s)
       (vui-bench--measure
        8 (lambda () (vui-stream-append handle (vui-text "freshly streamed line")))))
      (vui-bench--agent-teardown inst buf))))

(defun vui-bench-stream-update-last-growth ()
  "Cost of one `vui-stream-update-last' as the stream grows, box below.
The per-token path of a streaming agent UI (the in-progress message grows
word by word).  A FLAT slope means the update is independent of transcript
size - only the last item's region is rewritten."
  (vui-bench--header "Stream update-last: grow last item at stream size S (box below)")
  (dolist (s '(200 500 1000 2000 4000))
    (let* ((handle (vui-make-stream))
           (buf "*vui-bench-stream-ul*")
           (inst (vui-mount (vui-component 'vui-bench-stream-app :stream handle) buf))
           (k 0))
      (dotimes (i s)
        (vui-stream-append handle (vui-text (format "message %d content" i))))
      (vui-stream-append handle (vui-text "in progress"))
      (vui-bench--result-row
       (format "%d stream" s)
       (vui-bench--measure
        8 (lambda () (setq k (1+ k))
            (vui-stream-update-last
             handle (vui-text (format "growing message token %d here" k))))))
      (vui-bench--agent-teardown inst buf))))

(defun vui-bench-stream-update-last-extend ()
  "Cost of streaming ONE message of N chunks via update-last extension.
Each update EXTENDS the previous text, so the extend fast path runs once
per chunk.  The per-chunk prefix check must not allocate O(prefix), or
streaming a message of N chunks costs O(N^2) garbage in total (#127):
the tell is per-chunk cost and GC time rising with N.  The message-so-far
strings are precomputed so the timed run measures only update-last."
  (vui-bench--header "Stream update-last extend: stream one message of N chunks")
  ;; Two payloads: PLAIN chunks (the realistic case - streamed deltas are
  ;; plain strings, faces arrive via the :face prop) and FACED chunks (a
  ;; face-propertized span per chunk, so the prefix accumulates O(N)
  ;; property intervals - the worst case for a property-aware check).
  (dolist (payload `((plain . "token ")
                     (faced . ,(concat (propertize "tok" 'face 'shadow) "en "))))
    (dolist (n '(250 500 1000 2000))
      (let* ((handle (vui-make-stream))
             (buf "*vui-bench-stream-ext*")
             (inst (vui-mount (vui-component 'vui-bench-stream-app :stream handle)
                              buf))
             (chunk (cdr payload))
             ;; message-so-far vnodes, one per chunk count
             (vnodes (make-vector n nil)))
        (let ((text ""))
          (dotimes (i n)
            (setq text (concat text chunk))
            (aset vnodes i (vui-text text))))
        (vui-stream-append handle (vui-text "starting"))
        (vui-bench--result-row
         (format "%d %s" n (car payload))
         (vui-bench--measure
          5 (lambda ()
              ;; reset to a non-prefix (full re-render), then stream: every
              ;; subsequent update extends the last text by one chunk
              (vui-stream-update-last handle (vui-text "reset"))
              (dotimes (i n)
                (vui-stream-update-last handle (aref vnodes i))))))
        (vui-bench--agent-teardown inst buf)))))

(defun vui-bench-stream-box-update-growth ()
  "Cost of a box-only state change on the stream UI, flag off vs on.
Off rebuilds the whole transcript (O(N)); on leaves the stream region
untouched and re-renders only the box (the stream-tail patch), so the
slope should be FLAT - a box update is independent of transcript size."
  (vui-bench--header "Stream box update: toggle box state at stream size S")
  (dolist (s '(200 500 1000 2000 4000))
    (dolist (flag '((nil . "off") (t . "on")))
      (let* ((vui-incremental-render (car flag))
             (handle (vui-make-stream))
             (buf "*vui-bench-stream-box*")
             (inst (vui-mount (vui-component 'vui-agent-chat-stream
                                             :stream handle :queue 0 :status "idle")
                              buf))
             (tog nil))
        (dotimes (i s)
          (vui-stream-append handle (vui-text (format "message %d" i))))
        (vui-update inst (list :stream handle :queue 0 :status "idle"))
        (vui-bench--result-row
         (format "%d %s" s (cdr flag))
         (vui-bench--measure
          6 (lambda () (setq tog (not tog))
              (vui-update inst (list :stream handle :queue (if tog 1 0)
                                     :status (if tog "busy" "idle"))))))
        (vui-bench--agent-teardown inst buf)))))

(vui-defcomponent vui-bench-stream-row (id)
  :state ((open nil))
  :render (vui-vstack
           (vui-button (format "row %d %s" id (if open "v" ">"))
             :on-click (lambda () (vui-set-state :open (not open))))
           (when open (vui-text (format "detail for row %d, a few words here" id)))))

(defun vui-bench-stream-row-rerender-growth ()
  "Cost of toggling ONE stateful component row vs the items above it.
A row is an inline instance, so its state change re-renders only its own
region - this should be FLAT, independent of stream size, unlike walking
or rebuilding the transcript."
  (vui-bench--header "Stream row re-render: toggle 1 row at stream size S (box below)")
  (dolist (s '(200 500 1000 2000 4000))
    (let* ((handle (vui-make-stream))
           (buf "*vui-bench-stream-row*")
           (inst (vui-mount (vui-component 'vui-bench-stream-app :stream handle) buf)))
      (dotimes (i s)
        (vui-stream-append handle (vui-text (format "message %d content" i))))
      (vui-stream-append handle (vui-component 'vui-bench-stream-row :id 9999))
      (vui-bench--result-row
       (format "%d row" s)
       (vui-bench--measure
        8 (lambda ()
            (with-current-buffer buf
              (goto-char (point-min))
              (when (search-forward "row 9999" nil t)
                (let ((w (widget-at (match-beginning 0))))
                  (when w (widget-apply w :action))))))))
      (vui-bench--agent-teardown inst buf))))

(defun vui-bench-agent-run ()
  "Run the streaming-seam benchmarks (declarative baseline + vui-stream)."
  (interactive)
  (let ((vui-render-delay nil)
        (vui-timing-enabled nil)
        (vui-debug-enabled nil))
    (message "VUI streaming seam (Emacs %s)" emacs-version)
    (vui-bench-agent-append-growth)
    (vui-bench-agent-box-update)
    (vui-bench-stream-append-growth)
    (vui-bench-stream-update-last-growth)
    (vui-bench-stream-update-last-extend)
    (vui-bench-stream-box-update-growth)
    (vui-bench-stream-row-rerender-growth)
    (message "")
    (message "done.")))

;;; Cross-version / cross-flag comparison
;;
;; Answers three questions with one matrix, on whatever build is loaded
;; (master, or this branch). Feature-detected so the SAME file runs on
;; master - the flag-on and :memo variants simply do not appear there.
;;
;;   Q1 do the always-on changes cost anything?  master vs branch (flag off)
;;   Q2 what does the flag buy?                   branch off vs branch on
;;   Q3 what does :memo buy?                       naive vs should-update vs memo
;;
;; Run on each build and combine the CMPDATA lines:
;;   eldev emacs --batch -l benchmarks/vui-bench.el -f vui-bench-compare-run

(defvar vui-bench--memo-ok (fboundp 'vui--shallow-equal-plist)
  "Non-nil when the loaded vui supports :memo (this branch).")

(defvar vui-bench--incr-ok (boundp 'vui-incremental-render)
  "Non-nil when the loaded vui has the incremental-render flag.")

(defmacro vui-bench--with-flag (val &rest body)
  "Run BODY with `vui-incremental-render' bound to VAL, where supported.
On master (no such variable) BODY just runs - VAL is always nil there."
  (declare (indent 1))
  `(cl-progv (and vui-bench--incr-ok '(vui-incremental-render))
       (and vui-bench--incr-ok (list ,val))
     ,@body))

;; Three child strategies, identical output text - they differ only in
;; how much work a re-render does.  Each bumps the render counter so a
;; scenario can prove which children actually ran their render-fn.
(vui-defcomponent vui-bench-cmp-naive (id label)
  ;; no should-update: re-renders on every parent render
  :render (progn (cl-incf vui-bench--render-count)
                 (vui-text (format "[%s:%s]" id label))))

(vui-defcomponent vui-bench-cmp-su (id label)
  ;; hand-written should-update: bails when its label is unchanged
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (progn (cl-incf vui-bench--render-count)
                 (vui-text (format "[%s:%s]" id label))))

(when vui-bench--memo-ok
  ;; Deferred eval so master (no :memo keyword) never expands this macro.
  (eval '(vui-defcomponent vui-bench-cmp-memo (id label)
           :memo t
           :render (progn (cl-incf vui-bench--render-count)
                          (vui-text (format "[%s:%s]" id label))))
        t))

(vui-defcomponent vui-bench-cmp-list (items child)
  ;; CHILD is the component symbol to instantiate for each row
  :render (apply #'vui-vstack
                 (mapcar (lambda (it)
                           (vui-component child :key (car it) :id (car it)
                                          :label (cdr it)))
                         items)))

(defun vui-bench--cmp-flags ()
  "Flags to sweep: (LABEL . VALUE).  Just wholesale on master."
  (if vui-bench--incr-ok '(("off" . nil) ("on" . t)) '(("--" . nil))))

(defun vui-bench--cmp-strategies ()
  "Child strategies available on this build: (LABEL . COMPONENT-SYMBOL)."
  (append '(("naive" . vui-bench-cmp-naive)
            ("su"    . vui-bench-cmp-su))
          (when vui-bench--memo-ok '(("memo" . vui-bench-cmp-memo)))))

(defun vui-bench--cmp-ratio (results a b note)
  "Print and return the min-ratio of result B over result A from RESULTS.
RESULTS is an alist of LABEL -> stat.  NOTE labels the comparison.  Does
nothing when either cell is absent (e.g. on master)."
  (let ((sa (cdr (assoc a results)))
        (sb (cdr (assoc b results))))
    (when (and sa sb)
      (vui-bench--row (cons 30 (format "%s (%s vs %s)" note b a))
                      (cons 30 (vui-bench--ratio-note sb sa))))))

(defun vui-bench-compare ()
  "Localized 1-item change in an N-component list, every strategy x flag.
All cells are timed ROUND-ROBIN (see `vui-bench--compare') so system
drift cannot bias one variant over another - the flaw that made an
earlier sequential version show a 2x spread within a single cell.  Each
cell is first verified for PARITY (buffer matches a plain wholesale
rebuild) and MECHANISM (render-fn ran exactly as many times as the
strategy promises: 1 for su/memo, all N for naive), so a timing can only
mean what it claims.  Prints a matrix, the derived Q2/Q3 ratios, and
machine-readable CMPDATA lines."
  (vui-bench--header
   (format "Localized update matrix (N=2000) [%s]"
           (if vui-bench--incr-ok "branch" "master")))
  (vui-bench--row '(14 . "variant") '(34 . "median (min..max) +gc")
                  '(10 . "renders"))
  (let* ((n 2000)
         (items (vui-bench--items n))
         (mid (/ n 2))
         (alt (let ((c (copy-sequence items)))
                (setf (nth mid c) (cons (car (nth mid items)) "CHANGED"))
                c))
         ;; Reference output: a plain wholesale rebuild of the changed list.
         (expect-alt (with-temp-buffer
                       (vui--render-vnode
                        (apply #'vui-vstack
                               (mapcar (lambda (it)
                                         (vui-text (format "[%s:%s]"
                                                           (car it) (cdr it))))
                                       alt)))
                       (buffer-string)))
         (cells nil)   ; each: (label inst buf flag-val tog-cell rc)
         (specs nil))
    ;; Mount one persistent instance per cell, then verify parity + mechanism
    ;; up front (sequentially - render-count is global), recording the count.
    (dolist (strat (vui-bench--cmp-strategies))
      (dolist (flag (vui-bench--cmp-flags))
        (let* ((label (format "%s/%s" (car strat) (car flag)))
               (buf (format "*vui-bench-cmp-%s*" label))
               (child (cdr strat)))
          (vui-bench--with-flag (cdr flag)
            (let ((inst (vui-mount (vui-component 'vui-bench-cmp-list
                                                  :items items :child child)
                                   buf)))
              (with-current-buffer buf
                (setq vui-bench--render-count 0)
                (vui-update inst (list :items alt :child child))
                (let ((rc vui-bench--render-count)
                      (want (if (equal (car strat) "naive") n 1)))
                  (vui-bench--assert
                   (equal (buffer-string) expect-alt)
                   "%s parity: buffer != wholesale reference" label)
                  (vui-bench--assert
                   (= rc want) "%s mechanism: %d renders, expected %d"
                   label rc want)
                  (vui-update inst (list :items items :child child))
                  (push (list label inst buf (cdr flag) (list nil) rc child)
                        cells))))))))
    (setq cells (nreverse cells))
    ;; Build round-robin timing specs that each toggle their own instance
    ;; under their own flag.  Closing over the cell keeps state per-variant.
    (dolist (cell cells)
      (let ((inst (nth 1 cell)) (flag (nth 3 cell)) (tog (nth 4 cell))
            (child (nth 6 cell)))
        (push (list (nth 0 cell)
                    (lambda ()
                      (setcar tog (not (car tog)))
                      (vui-bench--with-flag flag
                        (vui-update inst (list :items (if (car tog) alt items)
                                               :child child)))))
              specs)))
    (setq specs (nreverse specs))
    (let ((results (vui-bench--compare vui-bench-rounds specs)))
      (dolist (cell cells)
        (let* ((label (nth 0 cell))
               (rc (nth 5 cell))
               (res (cdr (assoc label results))))
          (vui-bench--row (cons 14 label)
                          (cons 34 (concat (vui-bench--stat-cell res) " ms"))
                          (cons 10 rc))
          (message "CMPDATA localized %s %s %s %s %s %d"
                   label
                   (vui-bench--ms (vui-bench-stat-min res))
                   (vui-bench--ms (vui-bench-stat-median res))
                   (vui-bench--ms (vui-bench-stat-max res))
                   (vui-bench--ms (vui-bench-stat-gc res))
                   rc)))
      ;; Derived answers (within this build):
      (when vui-bench--incr-ok
        (message "")
        (vui-bench--cmp-ratio results "su/off" "su/on" "Q2 flag win")
        (vui-bench--cmp-ratio results "naive/off" "naive/on" "Q2 flag win")
        (vui-bench--cmp-ratio results "su/on" "memo/on" "Q3 memo vs su [~1.0]")
        (vui-bench--cmp-ratio results "naive/on" "memo/on" "Q3 memo vs naive"))
      ;; Clean up all the persistent cell instances.
      (dolist (cell cells)
        (vui-unmount (nth 1 cell))
        (when (get-buffer (nth 2 cell)) (kill-buffer (nth 2 cell)))))))

(defun vui-bench-compare-initial ()
  "Initial mount of an N-component list: regression check across versions."
  (vui-bench--header
   (format "Initial mount, N components [%s]"
           (if vui-bench--incr-ok "branch" "master")))
  (dolist (n '(500 2000))
    (cl-progv (and vui-bench--incr-ok '(vui-incremental-render)) '(nil)
      (let* ((items (vui-bench--items n))
             (buf "*vui-bench-cmp-init*")
             (res (vui-bench--measure
                   3 (lambda ()
                       (let ((i (vui-mount (vui-component 'vui-bench-cmp-list
                                                          :items items
                                                          :child 'vui-bench-cmp-su)
                                           buf)))
                         (vui-unmount i))))))
        (vui-bench--result-row (format "N=%d" n) res)
        (message "CMPDATA initial N=%d %s %s %s %s 0"
                 n (vui-bench--ms (vui-bench-stat-min res))
                 (vui-bench--ms (vui-bench-stat-median res))
                 (vui-bench--ms (vui-bench-stat-max res))
                 (vui-bench--ms (vui-bench-stat-gc res)))
        (when (get-buffer buf) (kill-buffer buf))))))

;;; Large-state :memo comparison
;;
;; The `:memo' bail-out compares props AND state with the
;; text-property-aware `vui--vnode-equal' (a Lisp walk) instead of the
;; C-level `equal' (#126, #128).  For props the cost was measured at the
;; time; state was only reasoned about: `prev-state' is a shallow copy
;; of the state plist, so unchanged values short-circuit on `eq' and the
;; walk never descends.  The one case where it runs to completion is a
;; state value replaced by a fresh but structurally equal object - a
;; functional update that rebuilt an equal structure.  These scenarios
;; pin numbers on that reasoning with a deliberately large state value
;; (a btop-like snapshot: 1000 process plists plus two 120-sample
;; histories), so a future change to the comparison has a baseline.
;;
;; Wholesale mode only: the comparison under test runs before commit,
;; so the incremental flag would only add unrelated commit-path noise.

(defun vui-bench--big-state ()
  "A btop-snapshot-shaped value: 1000 process plists + two histories.
Built fresh on every call, so two calls return `equal' structures that
share no cons cells, strings, or floats - the fresh-but-equal worst
case for change detection."
  (list :procs (cl-loop for i from 0 below 1000
                        collect (list :pid i
                                      :name (format "proc-%d" i)
                                      :user (if (= 0 (mod i 3)) "root" "app")
                                      :cpu (/ (mod (* 7 i) 1000) 10.0)
                                      :mem (* 3 i)
                                      :threads (1+ (mod i 32))
                                      :state (if (= 0 (mod i 5)) 'sleeping 'running)
                                      :prio 20))
        :cpu-history (cl-loop for i from 0 below 120 collect (mod (* 7 i) 100))
        :mem-history (cl-loop for i from 0 below 120 collect (mod (* 13 i) 100))))

(when vui-bench--memo-ok
  ;; Deferred eval so master (no :memo keyword) never expands this macro.
  (eval '(progn
           (vui-defcomponent vui-bench-memo-state-child (tag)
             :memo t
             :state ((snapshot nil) (tick 0))
             :render (progn
                       (cl-incf vui-bench--render-count)
                       (vui-text (format "%s tick=%d rows=%d" tag tick
                                         (length (plist-get snapshot :procs))))))
           (vui-defcomponent vui-bench-memo-state-parent (tag)
             :render (vui-component 'vui-bench-memo-state-child :tag tag)))
        t))

(defun vui-bench-memo-state ()
  "End-to-end flush cost around a `:memo' child holding a large state.
Three cells, timed round-robin on one mounted tree:

  eq-bail     parent re-renders, no state change: every state value is
              `eq', the walk never descends.  The common case; must
              stay cheap regardless of state size.
  tick        `vui-set-state' bumps a small int: the memo must NOT
              bail, and the comparison stops at the first difference.
              Cost here is a real child re-render, not comparison.
  equal-bail  `vui-set-state' swaps the snapshot for a fresh `equal'
              copy: the memo MUST bail, and only after walking the
              entire structure.  This is the worst case the scenario
              exists for.  Measured (Emacs 31, this state size): the
              flush is comparison-dominated - the walk alone (see the
              micro scenario) accounts for nearly all of the ~1.2ms,
              where the C `equal' walk it replaced took ~0.07ms.  That
              is the number that would justify future work (an
              identity-stable-state note in the docs, or a fast path)
              if a real UI ever hits this shape at a high refresh rate.

MECHANISM is asserted before and during timing: the render counter
proves each cell renders exactly as promised (0/1/0 per flush), and the
bail path is checked to refresh `prev-state', so every equal-bail toggle
really compares two fresh copies instead of degrading to `eq'."
  (when vui-bench--memo-ok
    (vui-bench--header "Large-state :memo bail-out (btop-snapshot shape)")
    (vui-bench--row '(12 . "cell") '(34 . "median (min..max) +gc")
                    '(10 . "renders"))
    (let* ((snap-a (vui-bench--big-state))
           (snap-b (vui-bench--big-state))
           (buf "*vui-bench-memo-state*")
           (inst (vui-mount (vui-component 'vui-bench-memo-state-parent
                                           :tag "s")
                            buf)))
      (vui-bench--assert (and (equal snap-a snap-b) (not (eq snap-a snap-b)))
                         "big-state copies must be equal but distinct")
      (unwind-protect
          (with-current-buffer buf
            (let ((child (car (vui-get-component-instances
                               'vui-bench-memo-state-child inst)))
                  (tog (list nil)))
              ;; Install the initial snapshot (untimed).
              (let ((vui--current-instance child))
                (vui-set-state :snapshot snap-a))
              ;; MECHANISM, one probe per cell before timing.
              ;; eq-bail: parent re-render, all state values eq -> child bails.
              (setq vui-bench--render-count 0)
              (vui-update-props inst '(:tag "s"))
              (vui-bench--assert (= vui-bench--render-count 0)
                                 "eq-bail: %d child renders, expected 0"
                                 vui-bench--render-count)
              ;; tick: small scalar changed -> child must NOT bail.
              (setq vui-bench--render-count 0)
              (let ((vui--current-instance child))
                (vui-set-state :tick #'1+))
              (vui-bench--assert (= vui-bench--render-count 1)
                                 "tick: %d child renders, expected 1"
                                 vui-bench--render-count)
              ;; equal-bail: fresh equal snapshot -> child bails, and the
              ;; bail must still refresh prev-state; otherwise the next
              ;; toggle back to the previous copy would short-circuit on
              ;; `eq' and the cell would measure the wrong path half the
              ;; time.
              (setq vui-bench--render-count 0)
              (let ((vui--current-instance child))
                (vui-set-state :snapshot snap-b))
              (vui-bench--assert (= vui-bench--render-count 0)
                                 "equal-bail: %d child renders, expected 0"
                                 vui-bench--render-count)
              (vui-bench--assert (eq (plist-get (vui-instance-prev-state child)
                                                :snapshot)
                                     snap-b)
                                 "equal-bail: prev-state not refreshed on bail")
              (let ((vui--current-instance child))
                (vui-set-state :snapshot snap-a))
              (vui-bench--assert (= vui-bench--render-count 0)
                                 "equal-bail: toggle back rendered the child")
              ;; Timing.  Round-robin across the cells; only the tick cell
              ;; renders, so the counter over the whole timed phase must be
              ;; exactly warmups + rounds.
              (setq vui-bench--render-count 0)
              (let* ((rounds vui-bench-rounds)
                     (results
                      (vui-bench--compare
                       rounds
                       (list
                        (list "eq-bail"
                              (lambda () (vui-update-props inst '(:tag "s"))))
                        (list "tick"
                              (lambda ()
                                (let ((vui--current-instance child))
                                  (vui-set-state :tick #'1+))))
                        (list "equal-bail"
                              (lambda ()
                                (setcar tog (not (car tog)))
                                (let ((vui--current-instance child))
                                  (vui-set-state :snapshot (if (car tog)
                                                               snap-b
                                                             snap-a)))))))))
                (vui-bench--assert (= vui-bench--render-count (+ 2 rounds))
                                   "timed phase: %d child renders, expected %d"
                                   vui-bench--render-count (+ 2 rounds))
                ;; Parity after the fact: the buffer must reflect exactly
                ;; the tick renders that happened (mechanism probe + timed
                ;; phase), with the bails leaving no trace.
                (vui-bench--assert
                 (equal (buffer-string)
                        (format "s tick=%d rows=1000" (+ 1 2 rounds)))
                 "buffer does not match the expected final render")
                (dolist (cell '("eq-bail" "tick" "equal-bail"))
                  (let ((res (cdr (assoc cell results))))
                    (vui-bench--row (cons 12 cell)
                                    (cons 34 (concat (vui-bench--stat-cell res)
                                                     " ms"))
                                    (cons 10 (if (equal cell "tick") 1 0)))
                    (message "CMPDATA memo-state %s %s %s %s %s"
                             cell
                             (vui-bench--ms (vui-bench-stat-min res))
                             (vui-bench--ms (vui-bench-stat-median res))
                             (vui-bench--ms (vui-bench-stat-max res))
                             (vui-bench--ms (vui-bench-stat-gc res)))))
                (vui-bench--cmp-ratio results "equal-bail" "eq-bail"
                                      "eq fast path vs full walk"))))
        (vui-unmount inst)
        (when (get-buffer buf) (kill-buffer buf))))))

(defun vui-bench-memo-state-micro ()
  "Direct cost of one comparison walk over the big state plist.
Times plain `equal' against the property-aware comparison the memo
bail-out actually uses (`vui--vnode-equal' in its equal-functions
mode), on two fresh `equal' builds, so the constant factor between the
C walk and the Lisp walk is a printed number.  The Lisp function is
byte-compiled (it comes from vui.elc); each sample runs the comparison
100 times.  The vnode-equal cell only appears on builds where the
comparison takes the extra mode argument."
  (vui-bench--header "Micro: equal vs vui--vnode-equal on the big state")
  (let* ((a (list :snapshot (vui-bench--big-state) :tick 0))
         (b (list :snapshot (vui-bench--big-state) :tick 0))
         (reps 100)
         ;; Late-bound so byte-compiling this file against a build whose
         ;; `vui--vnode-equal' still takes two arguments does not warn.
         (vnode-equal (and (fboundp 'vui--vnode-equal)
                           (>= (cdr (func-arity
                                     (symbol-function 'vui--vnode-equal)))
                               3)
                           (symbol-function 'vui--vnode-equal))))
    (vui-bench--assert (and (equal a b) (not (eq a b)))
                       "micro: builds must be equal but distinct")
    (when vnode-equal
      (vui-bench--assert (funcall vnode-equal a b t)
                         "micro: vnode-equal disagrees with equal"))
    ;; The comparison results land in `sink': `equal' is pure, and a
    ;; byte-compiled thunk discarding its value could have the call
    ;; optimized away entirely, timing an empty loop.
    (let* ((sink nil)
           (specs (append
                   (list (list "equal"
                               (lambda ()
                                 (dotimes (_ reps) (setq sink (equal a b))))))
                   (when vnode-equal
                     (list (list "vnode-eq"
                                 (lambda ()
                                   (dotimes (_ reps)
                                     (setq sink (funcall vnode-equal a b t)))))))))
           (results (vui-bench--compare vui-bench-rounds specs)))
      (vui-bench--assert sink "micro: comparison reported not-equal")
      (dolist (s specs)
        (let ((res (cdr (assoc (car s) results))))
          (vui-bench--row (cons 12 (car s))
                          (cons 34 (concat (vui-bench--stat-cell res) " ms"))
                          (cons 22 (format "%.1f us/walk"
                                           (/ (* 1e6 (vui-bench-stat-min res))
                                              reps))))
          (message "CMPDATA memo-state-micro %s %s %s %s %s"
                   (car s)
                   (vui-bench--ms (vui-bench-stat-min res))
                   (vui-bench--ms (vui-bench-stat-median res))
                   (vui-bench--ms (vui-bench-stat-max res))
                   (vui-bench--ms (vui-bench-stat-gc res)))))
      (vui-bench--cmp-ratio results "vnode-eq" "equal"
                            "C walk vs Lisp walk"))))

(defun vui-bench-memo-state-run ()
  "Run only the large-state :memo comparison benchmarks."
  (interactive)
  (let ((vui-render-delay nil)
        (vui-timing-enabled nil)
        (vui-debug-enabled nil))
    (message "VUI large-state :memo benchmarks (Emacs %s, %s build)"
             emacs-version (if vui-bench--incr-ok "branch" "master"))
    (vui-bench-memo-state)
    (vui-bench-memo-state-micro)
    (message "")
    (message "done.")))

(defun vui-bench-compare-run ()
  "Run only the cross-version comparison (mount + localized matrix)."
  (interactive)
  (let ((vui-render-delay nil)
        (vui-timing-enabled nil)
        (vui-debug-enabled nil))
    (message "VUI comparison (Emacs %s, %s build)"
             emacs-version (if vui-bench--incr-ok "branch" "master"))
    (vui-bench-compare-initial)
    (vui-bench-compare)
    (message "")
    (message "done.")))

;; Back-compat alias for the entry point referenced in docs/changelog.
(defalias 'vui-bench-compare-all #'vui-bench-compare-run)

(defun vui-bench-run ()
  "Run the full vui benchmark suite and print a report."
  (interactive)
  (let ((vui-render-delay nil)
        (vui-timing-enabled nil)
        (vui-debug-enabled nil))
    (message "VUI benchmark suite (Emacs %s)" emacs-version)
    (message "(min of K runs after warmup; gc = GC time of fastest run)")
    (vui-bench-initial-render)
    (vui-bench-rerender-unchanged)
    (vui-bench-rerender-all-changed)
    (vui-bench-localized-update)
    (vui-bench-reorder)
    (vui-bench-streaming)
    (vui-bench-throughput)
    (vui-bench-skip-knob)
    (vui-bench-widgets)
    (vui-bench-table)
    (vui-bench-table-columns)
    (vui-bench-table-cells)
    (vui-bench-table-width-mode)
    (vui-bench-component-bailout)
    (vui-bench-memo-state)
    (vui-bench-memo-state-micro)
    (vui-bench-streaming-growth)
    (vui-bench-agent-append-growth)
    (vui-bench-agent-box-update)
    (vui-bench-stream-append-growth)
    (vui-bench-stream-update-last-growth)
    (vui-bench-stream-update-last-extend)
    (vui-bench-stream-box-update-growth)
    (vui-bench-stream-row-rerender-growth)
    (message "")
    (message "done.")))

(provide 'vui-bench)
;;; vui-bench.el ends here

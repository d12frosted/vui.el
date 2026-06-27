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
    (vui-bench-component-bailout)
    (vui-bench-streaming-growth)
    (message "")
    (message "done.")))

(provide 'vui-bench)
;;; vui-bench.el ends here

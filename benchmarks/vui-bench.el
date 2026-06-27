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
;; Methodology: each measurement warms up once (untimed), then times K
;; runs and reports the MINIMUM wall time (least noisy estimator for "is
;; A faster than B") plus the GC time of that run, so allocation-driven
;; cost is visible.
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

(defun vui-bench--measure (k thunk)
  "Warm up THUNK once (untimed), then time it K times.
Return (MIN-SECONDS . GC-SECONDS-OF-FASTEST-RUN)."
  (funcall thunk)
  (let ((best most-positive-fixnum)
        (best-gc 0.0))
    (dotimes (_ k)
      (garbage-collect)
      (let ((r (benchmark-run 1 (funcall thunk))))
        (when (< (nth 0 r) best)
          (setq best (nth 0 r)
                best-gc (nth 2 r)))))
    (cons best best-gc)))

(defun vui-bench--result-row (label res)
  "Print LABEL and a measurement RES (cons of seconds . gc-seconds)."
  (vui-bench--row (cons 9 label)
                  (cons 13 (concat (vui-bench--ms (car res)) " ms"))
                  (cons 12 (concat (vui-bench--ms (cdr res)) " gc"))))

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
                    (cons 14 (concat (vui-bench--ms (car res)) " ms")))
    (vui-bench--row '(16 . "per update")
                    (cons 14 (concat (vui-bench--ms (/ (car res) 2000.0)) " ms")))))

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
    (message "")
    (message "done.")))

(provide 'vui-bench)
;;; vui-bench.el ends here

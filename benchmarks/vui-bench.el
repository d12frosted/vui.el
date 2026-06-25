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
;; These are not run by `eldev test' (they are slow and noisy). Run them
;; explicitly:
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
  "Print a table row from CELLS (each a cons of (WIDTH . STRING))."
  (message "%s"
           (mapconcat (lambda (c)
                        (let ((w (car c)) (s (format "%s" (cdr c))))
                          (concat s (make-string (max 0 (- w (length s))) ?\s))))
                      cells "  ")))

(defmacro vui-bench--elapse (&rest body)
  "Run BODY once after a GC, return elapsed seconds."
  `(progn (garbage-collect) (car (benchmark-run 1 ,@body))))

;;; Components used by the scenarios

(vui-defcomponent vui-bench-text-list (n)
  :render (vui-list (number-sequence 1 (or n 0))
                    (lambda (i)
                      (vui-text (format "row %d - representative line of content here" i)))
                    #'identity))

(vui-defcomponent vui-bench-item-list (n)
  :state ((items (cl-loop for i from 1 to (or n 0)
                          collect (list :id i :label (format "row %d - content" i)))))
  :render (vui-list items
                    (lambda (it) (vui-text (plist-get it :label)))
                    (lambda (it) (plist-get it :id))))

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

;;; Scenarios

(defconst vui-bench--sizes '(50 200 500 1000 2000 4000)
  "Item counts swept by the scaling scenarios.")

(defun vui-bench-initial-render ()
  "Initial mount cost vs item count."
  (vui-bench--header "Initial render (mount N text rows)")
  (vui-bench--row '(10 . "N") '(14 . "total/mount"))
  (dolist (n vui-bench--sizes)
    (let* ((buf (format "*vui-bench-init-%d*" n))
           (el (/ (vui-bench--elapse
                   (dotimes (_ 3) (vui-mount (vui-component 'vui-bench-text-list :n n) buf)))
                  3.0)))
      (vui-unmount buf)
      (when (get-buffer buf) (kill-buffer buf))
      (vui-bench--row (cons 10 n) (cons 14 (concat (vui-bench--ms el) " ms"))))))

(defun vui-bench-rerender ()
  "Full re-render cost vs item count (tree unchanged)."
  (vui-bench--header "Re-render (full re-render of N text rows)")
  (vui-bench--row '(10 . "N") '(14 . "per re-render"))
  (dolist (n vui-bench--sizes)
    (let* ((buf (format "*vui-bench-rr-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-text-list :n n) buf))
           (el (/ (vui-bench--elapse (dotimes (_ 5) (vui--rerender-instance inst))) 5.0)))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf))
      (vui-bench--row (cons 10 n) (cons 14 (concat (vui-bench--ms el) " ms"))))))

(defun vui-bench-single-update ()
  "Cost of changing ONE item in a list of N, then re-rendering."
  (vui-bench--header "Single-item update in a list of N")
  (vui-bench--row '(10 . "N") '(14 . "per update"))
  (dolist (n vui-bench--sizes)
    (let* ((buf (format "*vui-bench-su-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-item-list :n n) buf))
           (toggle 0)
           (el (/ (vui-bench--elapse
                   (dotimes (_ 5)
                     (let ((vui--current-instance inst))
                       (setq toggle (1+ toggle))
                       ;; replace one item with a fresh label, then render
                       (vui-set-state
                        :items
                        (lambda (items)
                          (let ((copy (copy-sequence items)))
                            (setcar copy (list :id 1 :label (format "row 1 - %d" toggle)))
                            copy))))))
                  5.0)))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf))
      (vui-bench--row (cons 10 n) (cons 14 (concat (vui-bench--ms el) " ms"))))))

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
      (let ((el (vui-bench--elapse
                 (let ((vui--current-instance inst))
                   (vui-set-state :lines (lambda (old) (cons line old)))))))
        (setq total (+ total el))
        (when (memq (1+ i) samples)
          (vui-bench--row (cons 10 (1+ i))
                          (cons 14 (concat (vui-bench--ms el) " ms"))
                          (cons 14 (concat (vui-bench--ms total) " ms"))))))
    (vui-unmount inst)
    (when (get-buffer buf) (kill-buffer buf))))

(defun vui-bench-throughput ()
  "Raw re-render throughput for a trivial UI (lower bound per render)."
  (vui-bench--header "Update throughput (trivial counter, 2000 updates)")
  (let* ((buf "*vui-bench-tp*")
         (inst (vui-mount (vui-component 'vui-bench-counter) buf))
         (el (vui-bench--elapse
              (let ((vui--current-instance inst))
                (dotimes (_ 2000) (vui-set-state :n #'1+))))))
    (vui-unmount inst)
    (when (get-buffer buf) (kill-buffer buf))
    (vui-bench--row '(16 . "2000 updates") (cons 14 (concat (vui-bench--ms el) " ms total")))
    (vui-bench--row '(16 . "per update") (cons 14 (concat (vui-bench--ms (/ el 2000.0)) " ms")))))

(defun vui-bench-widgets ()
  "Re-render cost vs interactive widget count (buttons)."
  (vui-bench--header "Widgets (full re-render of N buttons)")
  (vui-bench--row '(10 . "N") '(14 . "per re-render"))
  (dolist (n '(50 200 500 1000))
    (let* ((buf (format "*vui-bench-w-%d*" n))
           (inst (vui-mount (vui-component 'vui-bench-button-list :n n) buf))
           (el (/ (vui-bench--elapse (dotimes (_ 5) (vui--rerender-instance inst))) 5.0)))
      (vui-unmount inst)
      (when (get-buffer buf) (kill-buffer buf))
      (vui-bench--row (cons 10 n) (cons 14 (concat (vui-bench--ms el) " ms"))))))

;;;###autoload
(defun vui-bench-run ()
  "Run the full vui benchmark suite and print a report."
  (interactive)
  (let ((vui-render-delay nil)
        (vui-timing-enabled nil)
        (vui-debug-enabled nil))
    (message "VUI benchmark suite (Emacs %s)" emacs-version)
    (vui-bench-initial-render)
    (vui-bench-rerender)
    (vui-bench-single-update)
    (vui-bench-streaming)
    (vui-bench-throughput)
    (vui-bench-widgets)
    (message "")
    (message "done.")))

(provide 'vui-bench)
;;; vui-bench.el ends here

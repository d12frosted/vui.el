;;; vui-stream-node-fuzz-test.el --- randomized differential node test -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A randomized differential test for the `vui-stream' node API.  It drives
;; long, seeded sequences of mixed operations - static append, open,
;; append-to, update, update-last, finalize, before, after, remove - against
;; the live buffer while maintaining a trivially-correct MODEL (an ordered
;; list of the expected text lines).  After every single operation the buffer
;; must equal what a plain list of those lines renders (the oracle).
;;
;; This is the catch-all guard for the fiddly invariants the targeted specs
;; check one at a time: separator bookkeeping in before/after/remove, marker
;; insertion types, region-end/last-start sync at the bottom boundary, and
;; items-rev ordering.  Seeds are fixed so any failure reproduces, and the
;; failing op trace is reported.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(vui-defcomponent vui-snfz-app (stream)
  :render (vui-vstack (vui-stream stream) (vui-text "note: n0")))

(defun vui-snfz--oracle (texts)
  "Expected buffer for TEXTS stacked above the fixed note line."
  (if texts (concat (mapconcat #'identity texts "\n") "\nnote: n0") "note: n0"))

(defun vui-snfz--mount (s)
  (vui-mount (vui-component 'vui-snfz-app :stream s) "*vui-snfz*"))

(defun vui-snfz--buf ()
  (with-current-buffer "*vui-snfz*" (buffer-string)))

(defun vui-snfz--kill ()
  (when (get-buffer "*vui-snfz*") (kill-buffer "*vui-snfz*")))

(defun vui-snfz--insert-rel (lines id newcell after)
  "Return LINES with NEWCELL inserted before (or AFTER non-nil) the ID entry."
  (let (out)
    (dolist (c lines)
      (if (eq (car c) id)
          (if after (progn (push c out) (push newcell out))
            (progn (push newcell out) (push c out)))
        (push c out)))
    (nreverse out)))

(describe "vui-stream nodes: randomized differential vs a plain-list oracle"
  (it "stays byte-identical across mixed seeded op sequences"
    (let ((vui-render-delay nil)
          (failure nil))
      (cl-block done
        (dotimes (seed 80)
          ;; Deterministic per-seed RNG so any failure reproduces exactly.
          (random (format "vui-stream-node-fuzz-%d" seed))
          (let ((s (vui-make-stream))
                (lines '())        ; ordered (id . text), top to bottom
                (live '())         ; alist id -> node ref
                (next 0)
                (trace '()))
            (vui-snfz--mount s)
            (unwind-protect
                (dotimes (step 50)
                  (let* ((last-live (and lines
                                         (assq (car (car (last lines))) live)))
                         (ops (append '(append open)
                                      (when last-live '(update-last))
                                      (when live '(append-to update finalize
                                                   before after remove))))
                         (op (nth (random (length ops)) ops))
                         (pick (and live (nth (random (length live)) live)))
                         (pid (car pick))
                         (pnode (cdr pick))
                         (txt (format "x%d_%d" step (random 100))))
                    (push (list op pid txt) trace)
                    (pcase op
                      ('append
                       (vui-stream-append s (vui-text txt))
                       (setq lines (append lines (list (cons next txt)))
                             next (1+ next)))
                      ('open
                       (let ((n (vui-stream-open s (vui-text txt))))
                         (setq lines (append lines (list (cons next txt))))
                         (push (cons next n) live)
                         (setq next (1+ next))))
                      ('update-last
                       (vui-stream-update-last s (vui-text txt))
                       (setcdr (car (last lines)) txt))
                      ('append-to
                       (vui-stream-append-to pnode (vui-text txt))
                       (let ((c (assq pid lines))) (setcdr c (concat (cdr c) txt))))
                      ('update
                       (vui-stream-update pnode (vui-text txt))
                       (setcdr (assq pid lines) txt))
                      ('finalize
                       (vui-stream-finalize pnode)
                       (setq live (assq-delete-all pid live)))
                      ('before
                       (let ((n (vui-stream-before pnode (vui-text txt))))
                         (setq lines (vui-snfz--insert-rel lines pid (cons next txt) nil))
                         (push (cons next n) live)
                         (setq next (1+ next))))
                      ('after
                       (let ((n (vui-stream-after pnode (vui-text txt))))
                         (setq lines (vui-snfz--insert-rel lines pid (cons next txt) t))
                         (push (cons next n) live)
                         (setq next (1+ next))))
                      ('remove
                       (vui-stream-remove pnode)
                       (setq lines (assq-delete-all pid lines))
                       (setq live (assq-delete-all pid live))))
                    (let ((exp (vui-snfz--oracle (mapcar #'cdr lines)))
                          (got (vui-snfz--buf)))
                      (unless (equal exp got)
                        (setq failure (list :seed seed :step step
                                            :trace (reverse trace)
                                            :expected exp :got got))
                        (cl-return-from done)))))
              (vui-snfz--kill)))))
      (expect failure :to-be nil))))

(provide 'vui-stream-node-fuzz-test)
;;; vui-stream-node-fuzz-test.el ends here

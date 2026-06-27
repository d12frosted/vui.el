;;; vui-reconcile-test.el --- Child reconciliation lookup invariants -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Characterization tests for child reconciliation: which existing child
;; instance gets reused for each new child vnode.  These pin the exact
;; reuse semantics so the O(1)-lookup rewrite of `vui--find-matching-child'
;; (replacing the per-child linear scan that made a flat list of S
;; children O(S^2) to reconcile) is provably behavior-preserving.
;;
;; The two rules being locked:
;;   - keyed children are reused by (type, key), regardless of position
;;     (so state follows the key across reorders and mid-list inserts);
;;   - unkeyed children are reused by position (type at the same index).

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(vui-defcomponent vui-rc-cell (id)
  "A trivial keyed/unkeyed list cell."
  :render (vui-text (format "%s" id)))

(vui-defcomponent vui-rc-parent (ids keyed)
  "Render IDS as `vui-rc-cell' children, keyed by id when KEYED."
  :render
  (apply #'vui-vstack
         (mapcar (lambda (i)
                   (if keyed
                       (vui-component 'vui-rc-cell :key i :id i)
                     (vui-component 'vui-rc-cell :id i)))
                 ids)))

(defun vui-rc--by-id (root)
  "Map each `vui-rc-cell' :id under ROOT to its instance object."
  (let ((h (make-hash-table :test 'eql)))
    (dolist (i (vui-get-component-instances 'vui-rc-cell root))
      (puthash (plist-get (vui-instance-props i) :id) i h))
    h))

(defun vui-rc--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(describe "reconcile: keyed children are reused by key"
  (it "keeps each key's exact instance across a full reverse (N=50)"
    (let* ((vui-render-delay nil)
           (n 50)
           (ids (number-sequence 1 n))
           (buf "*vui-rc-keyed*")
           (inst (vui-mount (vui-component 'vui-rc-parent :ids ids :keyed t) buf)))
      (unwind-protect
          (let ((before (vui-rc--by-id inst)))
            (vui-update inst (list :ids (reverse ids) :keyed t))
            (let ((after (vui-rc--by-id inst)))
              (expect (hash-table-count after) :to-equal n)
              (dolist (id ids)
                ;; same instance object reused for this key, not recreated
                (expect (gethash id after) :to-be (gethash id before)))))
        (vui-rc--kill buf))))

  (it "preserves existing instances when inserting at the front"
    (let* ((vui-render-delay nil)
           (ids (number-sequence 1 20))
           (buf "*vui-rc-insert*")
           (inst (vui-mount (vui-component 'vui-rc-parent :ids ids :keyed t) buf)))
      (unwind-protect
          (let ((before (vui-rc--by-id inst)))
            (vui-update inst (list :ids (cons 0 ids) :keyed t))
            (let ((after (vui-rc--by-id inst)))
              (dolist (id ids)
                (expect (gethash id after) :to-be (gethash id before)))
              ;; the inserted key gets a fresh instance
              (expect (gethash 0 after) :not :to-be nil)
              (expect (gethash 0 before) :to-be nil)))
        (vui-rc--kill buf)))))

(describe "reconcile: unkeyed children are reused by position"
  (it "reuses the same N instances when ids change but count is stable"
    (let* ((vui-render-delay nil)
           (n 40)
           (ids (number-sequence 1 n))
           (buf "*vui-rc-pos*")
           (inst (vui-mount (vui-component 'vui-rc-parent :ids ids :keyed nil) buf)))
      (unwind-protect
          (let ((before (vui-get-component-instances 'vui-rc-cell inst)))
            (expect (length before) :to-equal n)
            ;; change every id; unkeyed => reuse by position, no recreation
            (vui-update inst (list :ids (mapcar (lambda (i) (+ 100 i)) ids)
                                   :keyed nil))
            (let ((after (vui-get-component-instances 'vui-rc-cell inst)))
              (expect (length after) :to-equal n)
              (dolist (a after)
                (expect (memq a before) :not :to-be nil))))
        (vui-rc--kill buf)))))

(provide 'vui-reconcile-test)
;;; vui-reconcile-test.el ends here

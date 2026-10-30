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
;;   - unkeyed children are reused by position among the unkeyed
;;     children (type at the same index); keyed and unkeyed children
;;     never share an instance.

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
  "Map each cell :id under ROOT to its instance object."
  (let ((h (make-hash-table :test 'eql)))
    (dolist (i (append (vui-get-component-instances 'vui-rc-cell root)
                       (vui-get-component-instances 'vui-rc-memo-cell root)))
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

;; Unkeyed cells from ITEMS, then a cell keyed `tail'
(vui-defcomponent vui-rc-tail-parent (items)
  :render (vui-fragment
           (mapcar (lambda (i) (vui-component 'vui-rc-cell :id i)) items)
           (vui-component 'vui-rc-cell :key 'tail :id 'tail)))

;; Cells keyed by ITEMS, then an unkeyed footer cell
(vui-defcomponent vui-rc-footer-parent (items)
  :render (vui-fragment
           (mapcar (lambda (i) (vui-component 'vui-rc-cell :key i :id i)) items)
           (vui-component 'vui-rc-cell :id 'footer)))

;; The same, as one flat vstack of components: the shape the incremental
;; renderer patches in place
(vui-defcomponent vui-rc-flat-parent (items)
  :render (apply #'vui-vstack
                 (append
                  (mapcar (lambda (i) (vui-component 'vui-rc-cell :key i :id i))
                          items)
                  (list (vui-component 'vui-rc-cell :id 'footer)))))

;; A cell that skips its re-render while its props stay the same, so the
;; incremental renderer leaves its text in place
(vui-defcomponent vui-rc-memo-cell (id)
  :memo t
  :render (vui-text (format "%s" id)))

;; ITEMS as memo cells, keyed where an item is a cons (KEY . ID)
(vui-defcomponent vui-rc-mixed-parent (items)
  :render (apply #'vui-vstack
                 (mapcar (lambda (i)
                           (if (consp i)
                               (vui-component 'vui-rc-memo-cell
                                              :key (car i) :id (cdr i))
                             (vui-component 'vui-rc-memo-cell :id i)))
                         items)))

(defun vui-rc--update-mixed (flag from to check)
  "Mount `vui-rc-mixed-parent' with items FROM and update it to TO.
`vui-incremental-render' is FLAG.  Call CHECK with the cells by id
before and after the update, and return the buffer text after it."
  (let* ((vui-render-delay nil)
         (vui-incremental-render flag)
         (buf "*vui-rc-mixed*")
         (inst (vui-mount (vui-component 'vui-rc-mixed-parent :items from)
                          buf)))
    (unwind-protect
        (progn
          ;; Mount leaves no render record: re-render once, so the update
          ;; below can patch the list in place
          (vui-update inst (list :items from))
          (spy-calls-reset 'vui--patch-component-list)
          (let ((before (vui-rc--by-id inst)))
            (vui-update inst (list :items to))
            (funcall check before (vui-rc--by-id inst))
            (with-current-buffer buf (buffer-string))))
      (vui-rc--kill buf))))

(describe "reconcile: keyed and unkeyed siblings"
  :var (warnings)
  (before-each
    (setq warnings nil)
    (spy-on 'display-warning :and-call-fake
            (lambda (_type message &rest _) (push message warnings))))

  (it "does not give a keyed instance to an unkeyed sibling"
    (let* ((vui-render-delay nil)
           (buf "*vui-rc-tail*")
           (inst (vui-mount (vui-component 'vui-rc-tail-parent :items '(a)) buf)))
      (unwind-protect
          (let ((tail (gethash 'tail (vui-rc--by-id inst))))
            ;; b lands at the index the tail had
            (vui-update inst (list :items '(a b)))
            (let ((after (vui-rc--by-id inst)))
              (expect (length (vui-instance-children inst)) :to-equal 3)
              (expect (gethash 'tail after) :to-be tail)
              (expect (gethash 'b after) :not :to-be tail)
              (expect warnings :to-equal nil)))
        (vui-rc--kill buf))))

  (it "keeps an unkeyed sibling when a keyed list before it grows"
    (let* ((vui-render-delay nil)
           (buf "*vui-rc-footer*")
           (inst (vui-mount (vui-component 'vui-rc-footer-parent :items '(a)) buf)))
      (unwind-protect
          (let ((before (vui-rc--by-id inst)))
            (vui-update inst (list :items '(z a b)))
            (let ((after (vui-rc--by-id inst)))
              (expect (gethash 'footer after) :to-be (gethash 'footer before))
              (expect (gethash 'a after) :to-be (gethash 'a before))))
        (vui-rc--kill buf))))

  (it "keeps an unkeyed sibling of a flat keyed list, patched in place"
    (let* ((vui-render-delay nil)
           (vui-incremental-render t)
           (buf "*vui-rc-flat*")
           (inst (vui-mount (vui-component 'vui-rc-flat-parent :items '(a)) buf)))
      (unwind-protect
          (let ((footer (gethash 'footer (vui-rc--by-id inst))))
            (spy-on 'vui--patch-component-list :and-call-through)
            (vui-update inst (list :items '(z a)))
            (expect (gethash 'footer (vui-rc--by-id inst)) :to-be footer)
            (vui-update inst (list :items nil))
            (expect (gethash 'footer (vui-rc--by-id inst)) :to-be footer)
            (expect 'vui--patch-component-list :to-have-been-called)
            (expect (with-current-buffer buf (buffer-string))
                    :to-equal "footer"))
        (vui-rc--kill buf))))

  (it "picks the same instances patched in place as rendered whole"
    (spy-on 'vui--patch-component-list :and-call-through)
    (dolist (flag '(nil t))
      (let ((x1 (vui-rc--update-mixed
                 flag '((k . a) x1) '(p q)
                 (lambda (before after)
                   ;; the first unkeyed vnode takes the only unkeyed
                   ;; instance
                   (expect (gethash 'p after) :to-be (gethash 'x1 before))
                   (expect (gethash 'q after) :not :to-be nil)
                   (expect (gethash 'q after) :not :to-be (gethash 'x1 before))))))
        (expect x1 :to-equal "p\nq")
        (expect (spy-calls-any 'vui--patch-component-list) :to-be flag)
        (spy-calls-reset 'vui--patch-component-list))))

  (it "keeps unkeyed siblings when a keyed one goes in before them"
    (spy-on 'vui--patch-component-list :and-call-through)
    (dolist (flag '(nil t))
      (expect (vui-rc--update-mixed
               flag '(x1 x2) '((k . k) x1 x2)
               (lambda (before after)
                 (expect (gethash 'x1 after) :to-be (gethash 'x1 before))
                 (expect (gethash 'x2 after) :to-be (gethash 'x2 before))))
              :to-equal "k\nx1\nx2")
      (expect (spy-calls-any 'vui--patch-component-list) :to-be flag)
      (spy-calls-reset 'vui--patch-component-list)))

  (it "keeps an unkeyed sibling when a keyed list before it shrinks"
    (let* ((vui-render-delay nil)
           (buf "*vui-rc-footer*")
           (inst (vui-mount (vui-component 'vui-rc-footer-parent :items '(a b)) buf)))
      (unwind-protect
          (let ((before (vui-rc--by-id inst)))
            (vui-update inst (list :items nil))
            (let ((after (vui-rc--by-id inst)))
              (expect (gethash 'footer after) :to-be (gethash 'footer before))
              (expect (gethash 'a before) :not :to-be (gethash 'footer after))
              (expect (vui-instance-mounted-p (gethash 'a before))
                      :to-equal 'unmounted)))
        (vui-rc--kill buf)))))

(provide 'vui-reconcile-test)
;;; vui-reconcile-test.el ends here

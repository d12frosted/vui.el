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
;; items-rev ordering.  The item ids (each item's step in the render path)
;; are checked along the way: they must stay with their items.  Seeds are
;; fixed so any failure reproduces, and the failing op trace is reported.

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
  ;; Without properties: a failure report shows the text, and the
  ;; properties of a button hold code that can break the formatting of
  ;; the report
  (with-current-buffer "*vui-snfz*"
    (buffer-substring-no-properties (point-min) (point-max))))

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
                          (got (vui-snfz--buf))
                          ;; The stream numbers its items the way this
                          ;; model does: one id per item, counted up
                          ;; from 0 as the items are made.  So its ids,
                          ;; top to bottom, are the model's, as long as
                          ;; every id stays with its item through each
                          ;; insert and remove.
                          (exp-ids (mapcar #'car lines))
                          (got-ids (reverse (vui-stream-handle-ids-rev s))))
                      (unless (and (equal exp got) (equal exp-ids got-ids))
                        (setq failure (list :seed seed :step step
                                            :trace (reverse trace)
                                            :expected exp :got got
                                            :expected-ids exp-ids
                                            :got-ids got-ids))
                        (cl-return-from done)))))
              (vui-snfz--kill)))))
      (expect failure :to-be nil))))

;; --- The same, across rebuilds of the stream, with component rows ---
;;
;; In the spec above the root renders again only when the stream gets its
;; first item or loses its last one, so the stream is never written again
;; while it has live nodes.  Here the root changes shape on the way: with
;; content before the stream every render writes the stream again from
;; its items, and the root also shows another tree (or nil) for a while
;; and then the stream again.  Component rows with state are mixed in.
;; After every operation the buffer must match the model, the item ids
;; must stay with their items, and every row must be mounted once.  While
;; the stream is shown, every live node and every row must also be bound
;; to its own text, and every row must start its renders from the path
;; of its item.
;;
;; While the stream is out of the buffer, an edit through a content node
;; writes nothing and is dropped (`vui-stream-before' and
;; `vui-stream-after' return nil).  A plain append, a newly opened node
;; and new props of a row are kept, and show when the stream is back.
;;
;; `vui-stream-update-last' on a static last item rewrites the text from
;; where the last item starts.  `vui-stream-remove' of the last item
;; moves that place to the item before it when that item is a live
;; content node, and forgets it otherwise.  Only an append or a rebuild
;; of the stream sets it again, so the model allows that case only when
;; the place is known.

(vui-defcomponent vui-snfz-row (label)
  "A row with state: a click counts up."
  :state ((n 0))
  :render (vui-button (format "%s/%d" label n)
            :on-click (lambda () (vui-set-state :n (1+ n)))))

(vui-defcomponent vui-snfz-shaped (stream mode label)
  "STREAM first (`tail'), after a header (`head'), or not shown at all."
  :render
  (pcase mode
    ('tail (vui-vstack (vui-stream stream)
                       (vui-text (format "note: %s" label))))
    ('head (vui-vstack (vui-text "header")
                       (vui-stream stream)
                       (vui-text (format "note: %s" label))))
    ;; A flat vstack of texts, so that with `vui-incremental-render' on
    ;; its commit takes the content branch, which writes the tree into
    ;; the erased buffer and parks the streams on its own
    ('other (vui-vstack (vui-text "other") (vui-text "more")))
    ('none nil)))

(defun vui-snfz--row-text (row)
  "The text ROW, a (NODE LABEL N) of the model, renders to."
  (format "[%s/%d]" (nth 1 row) (nth 2 row)))

(defun vui-snfz--bound-text (start end)
  "The text between markers START and END, or nil if they point nowhere."
  (and start end (marker-position start) (marker-position end)
       (with-current-buffer "*vui-snfz*"
         (buffer-substring-no-properties start end))))

(defun vui-snfz--shaped-oracle (mode away texts label)
  "Expected buffer of `vui-snfz-shaped' showing TEXTS, or AWAY instead."
  (pcase away
    ('other "other\nmore")
    ('none "")
    (_ (concat (if (eq mode 'head) "header\n" "")
               (mapconcat #'identity texts "\n")
               (if texts "\n" "")
               "note: " label))))

(describe "vui-stream nodes: randomized, across rebuilds of the stream"
  (it "keeps every live node and row bound to its own text"
    (let ((vui-render-delay nil)
          (failure nil))
      (cl-block done
        (dotimes (seed 60)
          (random (format "vui-stream-rebuild-fuzz-%d" seed))
          (let* ((vui-incremental-render (cl-oddp seed))
                 (s (vui-make-stream))
                 (mode 'tail)       ; the shape while the stream is shown
                 (away nil)         ; nil, or what the root shows instead
                 (label "l")
                 (lines '())        ; ordered (id . text), top to bottom
                 (live '())         ; alist id -> content node
                 (rows '())         ; alist id -> (node label n)
                 (next 0)
                 ;; Whether the stream knows where a static last item
                 ;; starts (see the commentary above)
                 (ul-ok t)
                 (trace '())
                 (root nil)
                 (show (lambda ()
                         (vui-update root (list :stream s :label label
                                                :mode (or away mode))))))
            (unwind-protect
                (dotimes (step 60)
                  (unless root
                    (setq root (vui-mount (vui-component 'vui-snfz-shaped
                                            :stream s :mode mode :label label)
                                          "*vui-snfz*")))
                  (let* ((last-id (car (car (last lines))))
                         (pick (and live (nth (random (length live)) live)))
                         (pid (car pick))
                         (pnode (cdr pick))
                         (row (and rows (nth (random (length rows)) rows)))
                         (ops (if away
                                  (append '(append open back back)
                                          (when live '(append-to update finalize
                                                       before after remove))
                                          (when rows '(update-row)))
                                (append '(append open relabel reshape leave)
                                        (when lines '(row))
                                        (when (or (assq last-id live)
                                                  (assq last-id rows)
                                                  (and lines ul-ok))
                                          '(update-last))
                                        (when live '(append-to update finalize
                                                     before after remove))
                                        (when rows '(click update-row)))))
                         (op (nth (random (length ops)) ops))
                         (txt (format "x%d_%d" step (random 100))))
                    ;; The id the op is about: the new item of append,
                    ;; open and row, the row of click and update-row, the
                    ;; last item of update-last, the node of a node op,
                    ;; and nil for the rest (`leave' puts in where the
                    ;; root went)
                    (push (list op
                                (pcase op
                                  ((or 'append 'open 'row) next)
                                  ((or 'click 'update-row) (car row))
                                  ('update-last last-id)
                                  ((or 'append-to 'update 'finalize
                                       'before 'after 'remove)
                                   pid))
                                txt)
                          trace)
                    (condition-case err
                        (pcase op
                          ('append
                           (vui-stream-append s (vui-text txt))
                           (setq lines (append lines (list (cons next txt)))
                                 next (1+ next)
                                 ul-ok t))
                          ('open
                           (let ((n (vui-stream-open s (vui-text txt))))
                             (setq lines (append lines (list (cons next txt))))
                             (push (cons next n) live)
                             (setq next (1+ next)
                                   ul-ok t)))
                          ('relabel
                           (setq label (format "l%d" step))
                           (funcall show)
                           ;; With a header the render rebuilds the stream
                           (when (eq mode 'head) (setq ul-ok t)))
                          ('reshape
                           (setq mode (if (eq mode 'tail) 'head 'tail))
                           (funcall show)
                           (setq ul-ok t))
                          ('leave
                           (setq away (if (zerop (random 2)) 'other 'none))
                           (setf (nth 1 (car trace)) away)
                           (funcall show))
                          ('back
                           (setq away nil)
                           (funcall show)
                           (setq ul-ok t))
                          ('row
                           (let ((r (list (vui-stream-open
                                           s (vui-component 'vui-snfz-row
                                               :label txt))
                                          txt 0)))
                             (push (cons next r) rows)
                             (setq lines (append lines
                                                 (list (cons next
                                                             (vui-snfz--row-text r))))
                                   next (1+ next)
                                   ul-ok t)))
                          ('click
                           (let ((r (cdr row)))
                             (with-current-buffer "*vui-snfz*"
                               (goto-char (point-min))
                               (search-forward (vui-snfz--row-text r))
                               (button-activate (button-at (match-beginning 0))))
                             (setf (nth 2 r) (1+ (nth 2 r)))
                             (setcdr (assq (car row) lines) (vui-snfz--row-text r))))
                          ('update-row
                           (let ((r (cdr row)))
                             (vui-stream-update
                              (nth 0 r) (vui-component 'vui-snfz-row :label txt))
                             (setf (nth 1 r) txt)
                             (setcdr (assq (car row) lines) (vui-snfz--row-text r))))
                          ('update-last
                           (if-let* ((r (cdr (assq last-id rows))))
                               (progn
                                 (vui-stream-update-last
                                  s (vui-component 'vui-snfz-row :label txt))
                                 (setf (nth 1 r) txt)
                                 (setcdr (car (last lines)) (vui-snfz--row-text r)))
                             (vui-stream-update-last s (vui-text txt))
                             (setcdr (car (last lines)) txt)))
                          ('append-to
                           (vui-stream-append-to pnode (vui-text txt))
                           (unless away
                             (let ((c (assq pid lines)))
                               (setcdr c (concat (cdr c) txt)))))
                          ('update
                           (vui-stream-update pnode (vui-text txt))
                           (unless away
                             (setcdr (assq pid lines) txt)))
                          ('finalize
                           (vui-stream-finalize pnode)
                           (setq live (assq-delete-all pid live)))
                          ('before
                           (let ((n (vui-stream-before pnode (vui-text txt))))
                             (if away
                                 (when n (error "Inserted while away: %S" n))
                               (setq lines (vui-snfz--insert-rel
                                            lines pid (cons next txt) nil))
                               (push (cons next n) live)
                               (setq next (1+ next)))))
                          ('after
                           (let ((n (vui-stream-after pnode (vui-text txt))))
                             (if away
                                 (when n (error "Inserted while away: %S" n))
                               (setq lines (vui-snfz--insert-rel
                                            lines pid (cons next txt) t))
                               (push (cons next n) live)
                               (setq next (1+ next)))))
                          ('remove
                           (vui-stream-remove pnode)
                           (unless away
                             (setq lines (assq-delete-all pid lines))
                             (setq live (assq-delete-all pid live))
                             (when (eq pid last-id)
                               (setq ul-ok (and (assq (car (car (last lines))) live)
                                                t))))))
                      ;; An operation that signals is a failure too, with
                      ;; the same report
                      (error
                       (setq failure (list :seed seed :step step
                                           :trace (reverse trace)
                                           :error err))
                       (cl-return-from done)))
                    (let* ((exp (vui-snfz--shaped-oracle
                                 mode away (mapcar #'cdr lines) label))
                           (got (vui-snfz--buf))
                           (exp-ids (mapcar #'car lines))
                           (got-ids (reverse (vui-stream-handle-ids-rev s)))
                           ;; Live nodes and rows that are not around their
                           ;; own text, and rows whose renders do not start
                           ;; from the render path of their item
                           (misplaced
                            (unless away
                              (append
                               (cl-remove-if
                                (lambda (l)
                                  (equal (vui-snfz--bound-text
                                          (vui--stream-node-start (cdr l))
                                          (vui--stream-node-end (cdr l)))
                                         (cdr (assq (car l) lines))))
                                live)
                               (cl-remove-if
                                (lambda (r)
                                  (let ((i (vui--stream-node-instance
                                            (nth 0 (cdr r)))))
                                    (and (equal (vui-snfz--bound-text
                                                 (vui-instance-region-start i)
                                                 (vui-instance-region-end i))
                                                (cdr (assq (car r) lines)))
                                         (equal (vui-instance-base-path i)
                                                (vui--stream-item-path
                                                 s (car r))))))
                                rows))))
                           ;; The mounted inline instances are the rows,
                           ;; each one once
                           (insts (with-current-buffer "*vui-snfz*"
                                    vui--inline-instances))
                           (mounted (length insts)))
                      (unless (and (equal exp got)
                                   (equal exp-ids got-ids)
                                   (null misplaced)
                                   (= mounted (length rows))
                                   (cl-every
                                    (lambda (r)
                                      (memq (vui--stream-node-instance
                                             (nth 0 (cdr r)))
                                            insts))
                                    rows))
                        (setq failure (list :seed seed :step step
                                            :trace (reverse trace)
                                            :expected exp :got got
                                            :expected-ids exp-ids
                                            :got-ids got-ids
                                            :misplaced (mapcar #'car misplaced)
                                            :mounted mounted))
                        (cl-return-from done)))))
              (vui-snfz--kill)))))
      (expect failure :to-be nil))))

(provide 'vui-stream-node-fuzz-test)
;;; vui-stream-node-fuzz-test.el ends here

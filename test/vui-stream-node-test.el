;;; vui-stream-node-test.el --- random-access vui-stream nodes -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Correctness invariants for the random-access node API (issue #82, the
;; design note docs/design/vui-stream-nodes.org).  `vui-stream-open'
;; returns a stable ref to one buffer region; that ref can be grown
;; (`vui-stream-append-to'), rewritten (`vui-stream-update'), or frozen
;; (`vui-stream-finalize') regardless of how many items land below it.
;; The bar is the same as the rest of vui-stream:
;;
;;   - after any sequence of node ops the buffer is BYTE-IDENTICAL to what
;;     a plain list of the same final items renders (the oracle);
;;   - updating or appending to a node that is NOT the last item is
;;     correct - items below it just shift;
;;   - the model (items-rev) stays in sync, so a forced wholesale
;;     re-render re-emits the streamed content byte-identically;
;;   - `vui-stream-finalize' actually releases the node's markers (the
;;     load-bearing invariant: a finalized node costs the buffer nothing,
;;     so append stays O(1) - see the design note's measurements).

;;; Code:

(require 'buttercup)
(require 'vui)

;; A stream above a declarative note line, same shape as vui-stream-test.
(vui-defcomponent vui-snode-app (stream note)
  :render
  (vui-vstack
   (vui-stream stream)
   (vui-text (format "note: %s" (or note "")))))

(defun vui-snode--oracle (lines note)
  "Expected buffer string: LINES stacked above the NOTE line."
  (if lines
      (concat (mapconcat #'identity lines "\n") "\nnote: " note)
    (concat "note: " note)))

(defun vui-snode--mount (stream note)
  (vui-mount (vui-component 'vui-snode-app :stream stream :note note)
             "*vui-snode*"))

(defun vui-snode--buffer ()
  (with-current-buffer "*vui-snode*" (buffer-string)))

(defun vui-snode--kill ()
  (when (get-buffer "*vui-snode*") (kill-buffer "*vui-snode*")))

(describe "vui-stream nodes: streaming into one ref"
  (it "append-to grows a node, byte-identical to the accumulated text"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            ;; non-empty first (open requires a live stream, like a row)
            (vui-stream-append s (vui-text "user: hi"))
            (let ((node (vui-stream-open s (vui-text "reply"))))
              (vui-stream-append-to node (vui-text " streaming"))
              (vui-stream-append-to node (vui-text " in"))
              (expect (vui-snode--buffer)
                      :to-equal
                      (vui-snode--oracle '("user: hi" "reply streaming in") "n0"))))
        (vui-snode--kill))))

  (it "append-to to a NON-last node lands in that node's region"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "first")))
            (vui-stream-open s (vui-text "second"))   ; a is no longer last
            (vui-stream-append-to a (vui-text "-grown"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "first-grown" "second") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order update"
  (it "updates a non-last node, grow then shrink, byte-identical each time"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "mid")))
            (vui-stream-open s (vui-text "last"))
            ;; grow the middle node
            (vui-stream-update a (vui-text "middle-is-now-long"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "middle-is-now-long" "last") "n0"))
            ;; shrink it
            (vui-stream-update a (vui-text "m"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "m" "last") "n0")))
        (vui-snode--kill))))

  (it "the last node updates correctly too (region-end stays at the true end)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (last)
            (vui-stream-append s (vui-text "header"))
            (setq last (vui-stream-open s (vui-text "x")))
            (vui-stream-update last (vui-text "x-grown"))
            ;; a plain static append after editing the last node must still
            ;; land at the true end, not inside the node
            (vui-stream-append s (vui-text "after"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "x-grown" "after") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: finalize releases the marker"
  (it "nulls the node's markers and drops it from the live set"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "done")))
            (expect (length (vui-stream-handle-nodes s)) :to-equal 1)
            (expect (marker-position (vui--stream-node-start node)) :not :to-be nil)
            (vui-stream-finalize node)
            ;; markers released - the load-bearing invariant
            (expect (vui--stream-node-start node) :to-be nil)
            (expect (vui--stream-node-end node) :to-be nil)
            (expect (vui-stream-handle-nodes s) :to-equal nil)
            ;; text stays exactly as rendered
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "done") "n0")))
        (vui-snode--kill))))

  (it "is a no-op to append-to / update a finalized node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "frozen")))
            (vui-stream-finalize node)
            (vui-stream-append-to node (vui-text " IGNORED"))
            (vui-stream-update node (vui-text "ALSO IGNORED"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "frozen") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: last-start stays valid for update-last"
  ;; A boundary node op (after/before/remove on the LAST item) must keep the
  ;; handle's last-start marker pointing at the real last item, or a later
  ;; non-extending `vui-stream-update-last' (content path) deletes the wrong
  ;; region.  The fast extend-path uses region-end and hid this.
  (it "after on the last node, then update-last, rewrites the right item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-after a (vui-text "X"))          ; header A X (X last)
            (vui-stream-update-last s (vui-text "Z"))    ; replace X, not A
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "Z") "n0")))
        (vui-snode--kill))))

  (it "before on the last node, then update-last, rewrites the right item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (l)
            (vui-stream-append s (vui-text "header"))
            (setq l (vui-stream-open s (vui-text "L")))
            (vui-stream-before l (vui-text "X"))         ; header X L (L last)
            (vui-stream-update-last s (vui-text "Q"))    ; replace L, not X
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "X" "Q") "n0")))
        (vui-snode--kill))))

  (it "remove of the last node, then update-last, rewrites the new last item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-remove b)                        ; header A (A last)
            (vui-stream-update-last s (vui-text "Q"))    ; replace A
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "Q") "n0")))
        (vui-snode--kill))))

  ;; A static last item is rewritten from last-start, which the remove
  ;; moved to A.  While A is a live node, update-last goes through the
  ;; node instead and never reads last-start.
  (it "remove of the last node, then update-last on the new last made static"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a b)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-remove b)                        ; header A (A last)
            (vui-stream-finalize a)                      ; A is static now
            (vui-stream-update-last s (vui-text "Q"))    ; replace A
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "Q") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: model stays in sync for re-emit"
  ;; The stream-tail patch is always on, so an in-place re-render leaves the
  ;; live region untouched and would exercise nothing.  Re-mounting the handle
  ;; into a fresh buffer renders the stream from scratch (`vui--stream-render'
  ;; over items-rev), genuinely checking the model.
  (it "re-mounting re-emits the accumulated streamed content"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "re")))
            (vui-stream-append-to node (vui-text "ply"))
            (vui-stream-append-to node (vui-text "-done"))
            (vui-stream-finalize node)
            (vui-snode--kill)
            (vui-snode--mount s "n0")
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "reply-done") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order insert"
  (it "before inserts a live node above another, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b x)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (setq x (vui-stream-before b (vui-text "X")))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "B") "n0"))
            ;; the inserted node is live - it can keep streaming
            (vui-stream-append-to x (vui-text "-grown"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "A" "X-grown" "B") "n0")))
        (vui-snode--kill))))

  (it "before the only node (opened on an empty stream) puts it on top"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "A")))   ; first item, via re-render
            (vui-stream-before a (vui-text "X"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("X" "A") "n0")))
        (vui-snode--kill))))

  (it "before a node that renders nothing leaves that node after it"
    ;; The node has no text: its start and its end are one position, and
    ;; both have to move past what is inserted there
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "")))
            (vui-stream-before a (vui-text "X"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "X" "") "n0"))
            (vui-stream-append-to a (vui-text "A"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "X" "A") "n0"))
            ;; the stream ends after the node, not before X
            (vui-stream-append s (vui-text "tail"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "X" "A" "tail") "n0")))
        (vui-snode--kill))))

  (it "before an empty node in the middle leaves the items below alone"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "")))
            (vui-stream-append s (vui-text "tail"))
            (vui-stream-before a (vui-text "X"))
            (vui-stream-update a (vui-text "A"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "X" "A" "tail") "n0")))
        (vui-snode--kill))))

  (it "after inserts a live node below another, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-open s (vui-text "B"))
            (vui-stream-after a (vui-text "X"))   ; between A and B
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "B") "n0")))
        (vui-snode--kill))))

  (it "after the last node keeps region-end at the true end"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-after a (vui-text "X"))   ; X is now the last item
            ;; a plain append must land after X, not inside the stream
            (vui-stream-append s (vui-text "tail"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "tail") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: remove"
  (it "removes a middle item, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-open s (vui-text "C"))
            (vui-stream-remove b)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "C") "n0")))
        (vui-snode--kill))))

  (it "removes the last item and a later append lands correctly"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-remove b)
            (vui-stream-append s (vui-text "tail"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "tail") "n0")))
        (vui-snode--kill))))

  (it "removes the first item, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "A")))   ; first item
            (vui-stream-open s (vui-text "B"))
            (vui-stream-remove a)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("B") "n0")))
        (vui-snode--kill))))

  (it "removes down to empty, then re-grows"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "only")))
            (vui-stream-remove a)
            (expect (vui-snode--buffer) :to-equal (vui-snode--oracle '() "n0"))
            (vui-stream-append s (vui-text "again"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("again") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order edits survive a wholesale re-render"
  (it "re-emits insert/remove results in the right order"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a b)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-after a (vui-text "X"))   ; header A X B
            (vui-stream-remove b)                 ; header A X
            ;; re-mount the handle -> render the model from scratch
            (vui-snode--kill)
            (vui-snode--mount s "n0")
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X") "n0")))
        (vui-snode--kill)))))

(vui-defcomponent vui-snode-row (label)
  "A trivial row that renders its LABEL, for component-node tests."
  :render (vui-text (format "[%s]" label)))

(describe "vui-stream nodes: component nodes (open a row)"
  (it "opens a component vnode as a live row node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open
                        s (vui-component 'vui-snode-row :label "a")))
            (expect (vui--stream-node-instance node) :not :to-be nil)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  (it "updates a component node OUT OF ORDER (row no longer last)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-append s (vui-text "below"))   ; row is no longer last
            (vui-stream-update row (vui-component 'vui-snode-row :label "AA"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[AA]" "below") "n0")))
        (vui-snode--kill))))

  (it "finalize stops tracking a component node but leaves the row live"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (expect (length (vui-stream-handle-nodes s)) :to-equal 1)
            (vui-stream-finalize row)
            (expect (vui-stream-handle-nodes s) :to-equal nil)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  ;; append-to is content-only: on a component node it must be a clean no-op,
  ;; NOT setcar a fragment over the live instance (which would crash a re-emit).
  (it "append-to on a component node is a clean no-op (items-rev stays sane)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-append-to row (vui-text "IGNORED"))
            ;; the live instance is still the items-rev head, not wrapped
            (expect (vui-instance-p (car (vui-stream-handle-items-rev s)))
                    :to-be t)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  ;; Opening a component on an EMPTY stream takes the row fallback (no inline
  ;; instance): the node must NOT capture the root instance, and a later
  ;; update must not hijack the buffer.
  (it "open of a component on an empty stream does not capture the root"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (setq node (vui-stream-open
                        s (vui-component 'vui-snode-row :label "solo")))
            (expect (vui--stream-node-instance node) :to-be nil)
            (vui-stream-update node (vui-component 'vui-snode-row :label "X"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[solo]") "n0")))
        (vui-snode--kill))))

  ;; After finalize the node is inert: update must not setcar a vnode over the
  ;; still-live row instance in items-rev.
  (it "update after finalize on a component node is a clean no-op"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-finalize row)
            (vui-stream-update row (vui-component 'vui-snode-row :label "ZZ"))
            (expect (vui-instance-p (car (vui-stream-handle-items-rev s)))
                    :to-be t)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill)))))

;; --- A rebuild of the stream re-binds its nodes ---
;;
;; A render that rewrites the stream erases the text of its nodes, and
;; the markers of a node end up together at the place of the erase.  The
;; rebuild must bind every live node to the text it writes for it, so
;; the node is edited in place afterwards.  While the stream is not in
;; the buffer, a node has no place to write to.

(defconst vui-snode-gap--buffer "*vui-snode-gap*")

(defvar vui-snode-gap--stream nil
  "The stream handle of the last render of `vui-snode-gap-root'.")

(vui-defcomponent vui-snode-gap-root (mode)
  :render
  (let ((h (vui-use-stream)))
    (setq vui-snode-gap--stream h)
    (pcase mode
      ('tail (vui-vstack (vui-stream h) (vui-text "footer")))
      ('head (vui-vstack (vui-text "header") (vui-stream h) (vui-text "footer")))
      ('other (vui-text "other"))
      ('none nil))))

(defun vui-snode-gap--mount (mode)
  (vui-mount (vui-component 'vui-snode-gap-root :mode mode)
             vui-snode-gap--buffer))

(defun vui-snode-gap--text ()
  (with-current-buffer vui-snode-gap--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vui-snode-gap--kill ()
  (when (get-buffer vui-snode-gap--buffer)
    (kill-buffer vui-snode-gap--buffer)))

(describe "vui-stream nodes: a rebuild of the stream re-binds them"
  (dolist (gap '(other none))
    (dolist (flag '(nil t))
      (it (format "edits a node in place after the root rendered %s (flag %s)"
                  (if (eq gap 'none) "nil" "another tree")
                  (if flag "on" "off"))
        (let ((vui-incremental-render flag)
              (vui-render-delay nil)
              (root (vui-snode-gap--mount 'tail)))
          (unwind-protect
              (let* ((s vui-snode-gap--stream)
                     (n (progn (vui-stream-append s (vui-text "a"))
                               (vui-stream-open s (vui-text "N1"))))
                     (r (vui-stream-open
                         s (vui-component 'vui-snode-row :label "r1"))))
                (vui-stream-append s (vui-text "b"))
                (vui-update root (list :mode gap))
                (vui-update root '(:mode tail))
                (expect (vui-snode-gap--text) :to-equal "a\nN1\n[r1]\nb\nfooter")
                (vui-stream-update n (vui-text "N2"))
                (expect (vui-snode-gap--text) :to-equal "a\nN2\n[r1]\nb\nfooter")
                (vui-stream-append-to n (vui-text "+"))
                (expect (vui-snode-gap--text) :to-equal "a\nN2+\n[r1]\nb\nfooter")
                (let ((pre (vui-stream-before n (vui-text "pre"))))
                  (vui-stream-after n (vui-text "post"))
                  (expect (vui-snode-gap--text)
                          :to-equal "a\npre\nN2+\npost\n[r1]\nb\nfooter")
                  (vui-stream-remove pre))
                (expect (vui-snode-gap--text)
                        :to-equal "a\nN2+\npost\n[r1]\nb\nfooter")
                ;; A component node is a row, re-seated by the rebuild
                (vui-stream-update r (vui-component 'vui-snode-row :label "r2"))
                (expect (vui-snode-gap--text)
                        :to-equal "a\nN2+\npost\n[r2]\nb\nfooter"))
            (vui-snode-gap--kill))))))

  (dolist (gap '(other none))
    (dolist (flag '(nil t))
      (it (format "writes nothing for a node while the root renders %s (flag %s)"
                  (if (eq gap 'none) "nil" "another tree")
                  (if flag "on" "off"))
        (let ((vui-incremental-render flag)
              (vui-render-delay nil)
              (root (vui-snode-gap--mount 'tail))
              (shown (if (eq gap 'none) "" "other")))
          (unwind-protect
              (let* ((s vui-snode-gap--stream)
                     (n (progn (vui-stream-append s (vui-text "a"))
                               (vui-stream-open s (vui-text "N1")))))
                (vui-stream-append s (vui-text "b"))
                (vui-update root (list :mode gap))
                (expect (vui-snode-gap--text) :to-equal shown)
                (vui-stream-update n (vui-text "N2"))
                (expect (vui-snode-gap--text) :to-equal shown)
                (vui-stream-append-to n (vui-text "+"))
                (expect (vui-snode-gap--text) :to-equal shown)
                (expect (vui-stream-before n (vui-text "pre")) :to-be nil)
                (expect (vui-stream-after n (vui-text "post")) :to-be nil)
                (expect (vui-snode-gap--text) :to-equal shown)
                (vui-stream-remove n)
                (expect (vui-snode-gap--text) :to-equal shown)
                ;; A node opened now gets its place when the stream is back
                (let ((late (vui-stream-open s (vui-text "late"))))
                  (expect (vui-snode-gap--text) :to-equal shown)
                  (vui-update root '(:mode tail))
                  (expect (vui-snode-gap--text)
                          :to-equal "a\nN1\nb\nlate\nfooter")
                  ;; Both nodes are live again
                  (vui-stream-update n (vui-text "N3"))
                  (vui-stream-update late (vui-text "LATE"))
                  (expect (vui-snode-gap--text)
                          :to-equal "a\nN3\nb\nLATE\nfooter")))
            (vui-snode-gap--kill))))))

  (dolist (flag '(nil t))
    (it (format "edits a node in place when the stream is not the first child (flag %s)"
                (if flag "on" "off"))
      ;; Every render of the root rebuilds the stream here
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-snode-gap--mount 'head)))
        (unwind-protect
            (let* ((s vui-snode-gap--stream)
                   (n (progn (vui-stream-append s (vui-text "a"))
                             (vui-stream-open s (vui-text "N1"))))
                   (m (progn (vui-stream-append s (vui-text "b"))
                             (vui-stream-open s (vui-text "M1")))))
              (vui-rerender root)
              (expect (vui-snode-gap--text)
                      :to-equal "header\na\nN1\nb\nM1\nfooter")
              (vui-stream-update n (vui-text "N2"))
              (expect (vui-snode-gap--text)
                      :to-equal "header\na\nN2\nb\nM1\nfooter")
              ;; The last node grows, and what is appended lands after it
              (vui-stream-append-to m (vui-text "+"))
              (vui-stream-update-last s (vui-text "M2+"))
              (vui-stream-append s (vui-text "c"))
              (expect (vui-snode-gap--text)
                      :to-equal "header\na\nN2\nb\nM2+\nc\nfooter")
              (vui-rerender root)
              (vui-stream-update n (vui-text "N3"))
              (vui-stream-update m (vui-text "M3"))
              (expect (vui-snode-gap--text)
                      :to-equal "header\na\nN3\nb\nM3\nc\nfooter"))
          (vui-snode-gap--kill))))))

(provide 'vui-stream-node-test)
;;; vui-stream-node-test.el ends here

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

(describe "vui-stream nodes: model stays in sync for re-emit"
  (it "a forced wholesale re-render reproduces the streamed content"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-snode--mount s "n0")))
        (unwind-protect
            (let (node)
              (vui-stream-append s (vui-text "header"))
              (setq node (vui-stream-open s (vui-text "re")))
              (vui-stream-append-to node (vui-text "ply"))
              (vui-stream-append-to node (vui-text "-done"))
              (vui-stream-finalize node)
              ;; Force the wholesale path (no incremental box-update patch):
              ;; it re-emits items-rev, which must carry the accumulated text.
              (let ((vui-incremental-render nil))
                (vui-rerender inst))
              (expect (vui-snode--buffer)
                      :to-equal
                      (vui-snode--oracle '("header" "reply-done") "n0")))
          (vui-snode--kill))))))

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
      (let ((inst (vui-snode--mount s "n0")))
        (unwind-protect
            (let (a b)
              (vui-stream-append s (vui-text "header"))
              (setq a (vui-stream-open s (vui-text "A")))
              (setq b (vui-stream-open s (vui-text "B")))
              (vui-stream-after a (vui-text "X"))   ; header A X B
              (vui-stream-remove b)                 ; header A X
              (let ((vui-incremental-render nil))
                (vui-rerender inst))
              (expect (vui-snode--buffer)
                      :to-equal (vui-snode--oracle '("header" "A" "X") "n0")))
          (vui-snode--kill))))))

(provide 'vui-stream-node-test)
;;; vui-stream-node-test.el ends here

;;; vui-stream-path-test.el --- Render paths of vui-stream items -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Every widget records its place in the tree as `:vui-path', and a
;; re-render finds the widget under point again by that path.  The items
;; of a `vui-stream' are siblings like the children of any container, so
;; each one needs its own step in the path.  These tests pin:
;;
;;   UNIQUE - no two items of a stream share a path, whichever way they
;;            got into the buffer (append, a node, a component row);
;;   STABLE - an item has the same path after the stream is rebuilt from
;;            its item list as it had from the imperative edit, also
;;            after items were inserted or removed around it;
;;   AROUND - widgets below a live stream have the same path whether the
;;            render patched around the stream or rebuilt everything.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

;; The stream is not the first child, so every re-render of the root
;; rebuilds it from its item list.
(vui-defcomponent vui-spath-head (stream label)
  :render (vui-vstack (vui-text "header")
                      (vui-stream stream)
                      (vui-text (format "status: %s" label))))

;; The stream is the first child, so a re-render patches around it and
;; only rewrites the buttons below.
(vui-defcomponent vui-spath-tail (stream label)
  :render (vui-vstack (vui-stream stream)
                      (vui-hstack (vui-button "send")
                                  (vui-button (format "stop %s" label)))))

;; A fragment keeps its nil children, so the stream is child 1 here and
;; the button child 3.
(vui-defcomponent vui-spath-frag (stream label)
  :render (vui-fragment nil
                        (vui-stream stream)
                        (vui-newline)
                        (vui-button (format "stop %s" label))))

;; The stream sits two containers deep in a child component.
(vui-defcomponent vui-spath-inner (stream)
  :render (vui-hstack (vui-text "|")
                      (vui-vstack (vui-text "log")
                                  (vui-stream stream))))

(vui-defcomponent vui-spath-nested (stream label)
  :render (vui-vstack (vui-text (format "status: %s" label))
                      (vui-component 'vui-spath-inner :stream stream)))

;; A row with state: the button toggles a detail line.
(vui-defcomponent vui-spath-row (id)
  :state ((open nil))
  :render (vui-vstack
           (vui-hstack (vui-text (format "row %s" id))
                       (vui-button "toggle"
                         :on-click (lambda () (vui-set-state :open (not open)))))
           (when open (vui-text "  detail"))))

(defconst vui-spath--buffer "*vui-spath*")

(defun vui-spath--item (text &optional label)
  "Return a stream item: TEXT followed by a button labelled LABEL."
  (vui-fragment (vui-text (concat text " "))
                (vui-button (or label "copy"))))

(defun vui-spath--mount (type stream)
  "Mount component TYPE around STREAM and return the instance."
  (vui-mount (vui-component type :stream stream :label "a")
             vui-spath--buffer))

(defun vui-spath--kill ()
  (when (get-buffer vui-spath--buffer)
    (kill-buffer vui-spath--buffer)))

(defun vui-spath--paths ()
  "Return (LINE LABEL PATH) for every button of the buffer, in order.
LINE is the text of the button's line, which tells the items apart when
their buttons share a label."
  (with-current-buffer vui-spath--buffer
    (mapcar (lambda (button)
              (list (save-excursion
                      (goto-char (button-start button))
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
                    (vui--elt-get button :vui-tag)
                    (vui--elt-get button :vui-path)))
            (vui--collect-widgets))))

(defun vui-spath--button (text)
  "Return the first button after TEXT in the buffer."
  (with-current-buffer vui-spath--buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward text)
      (next-button (point) t))))

(defun vui-spath--path (text)
  "Return the path of the first button after TEXT in the buffer."
  (vui--elt-get (vui-spath--button text) :vui-path))

(defun vui-spath--distinct-p (paths)
  "Non-nil when no path occurs twice in PATHS."
  (= (length paths) (length (delete-dups (copy-sequence paths)))))

(defun vui-spath--rebuild (instance stream &optional label)
  "Re-render INSTANCE around STREAM with LABEL, and check the stream rebuilt.
LABEL defaults to \"b\".  A marker inside the stream is thrown to the
top when the text under it is erased, which a render that patched
around the stream would not do."
  (with-current-buffer vui-spath--buffer
    (let ((probe (copy-marker (marker-position
                               (vui-stream-handle-region-end stream)))))
      (vui-update instance (list :stream stream :label (or label "b")))
      (expect (marker-position probe) :to-equal (point-min))
      (set-marker probe nil))))

(describe "vui-stream paths: one per item"
  (it "gives every appended item a path of its own"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      ;; Two items wait for the first render, three are appended live.
      (vui-stream-append s (vui-spath--item "m0"))
      (vui-stream-append s (vui-spath--item "m1"))
      (vui-spath--mount 'vui-spath-head s)
      (unwind-protect
          (progn
            (dotimes (i 3)
              (vui-stream-append s (vui-spath--item (format "m%d" (+ 2 i)))))
            ;; Child 1 of the root's vstack is the stream, then comes the
            ;; item, then the button as child 1 of the item's fragment.
            (expect (vui-spath--paths)
                    :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                ("m1 [copy]" "copy" (1 1 1))
                                ("m2 [copy]" "copy" (1 2 1))
                                ("m3 [copy]" "copy" (1 3 1))
                                ("m4 [copy]" "copy" (1 4 1)))))
        (vui-spath--kill))))

  (it "keeps every path when the stream is rebuilt"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (progn
            (dotimes (i 5)
              (vui-stream-append s (vui-spath--item (format "m%d" i))))
            (let ((live (vui-spath--paths)))
              (expect (length live) :to-equal 5)
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "starts every path at the path of the stream, however deep it is"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-nested s)))
      (unwind-protect
          (progn
            (dotimes (i 3)
              (vui-stream-append s (vui-spath--item (format "m%d" i))))
            ;; Child 1 of the root's vstack is the component.  In it the
            ;; vstack is child 1 of the hstack, and the stream child 1
            ;; of the vstack.
            (expect (mapcar #'caddr (vui-spath--paths))
                    :to-equal '((1 1 1 0 1) (1 1 1 1 1) (1 1 1 2 1)))
            (let ((live (vui-spath--paths)))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "keeps the path of the last item across update-last"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (progn
            (vui-stream-append s (vui-spath--item "m0"))
            (vui-stream-append s (vui-spath--item "m1"))
            (vui-stream-update-last s (vui-spath--item "m1 done" "retry"))
            (expect (vui-spath--paths)
                    :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                ("m1 done [retry]" "retry" (1 1 1))))
            (let ((live (vui-spath--paths)))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill)))))

(describe "vui-stream paths: nodes"
  (it "keeps a node's path when it is updated out of order"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq node (vui-stream-open s (vui-spath--item "running")))
            (vui-stream-append s (vui-spath--item "m2"))
            (expect (vui-spath--path "running") :to-equal '(1 1 1))
            (vui-stream-update node (vui-spath--item "result"))
            (expect (vui-spath--path "result") :to-equal '(1 1 1))
            (let ((live (vui-spath--paths)))
              (expect (vui-spath--distinct-p (mapcar #'caddr live)) :to-be t)
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "gives content added with append-to the path a rebuild gives it"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq node (vui-stream-open s (vui-spath--item "reply" "a")))
            (vui-stream-append-to node (vui-button "b"))
            (vui-stream-append-to node (vui-button "c"))
            (expect (vui-spath--paths)
                    :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                ("reply [a][b][c]" "a" (1 1 1))
                                ("reply [a][b][c]" "b" (1 1 2))
                                ("reply [a][b][c]" "c" (1 1 3))))
            (let ((live (vui-spath--paths)))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "gives a node inserted before or after another a path of its own"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (a b)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq a (vui-stream-open s (vui-spath--item "A")))
            (setq b (vui-stream-open s (vui-spath--item "B")))
            (vui-stream-before b (vui-spath--item "X"))
            (vui-stream-after a (vui-spath--item "Y"))
            (vui-stream-after b (vui-spath--item "Z"))
            (let ((live (vui-spath--paths)))
              (expect (mapcar #'car live)
                      :to-equal '("m0 [copy]" "A [copy]" "Y [copy]"
                                  "X [copy]" "B [copy]" "Z [copy]"))
              (expect (vui-spath--distinct-p (mapcar #'caddr live)) :to-be t)
              ;; The items that were there keep the path they had.
              (expect (vui-spath--path "A ") :to-equal '(1 1 1))
              (expect (vui-spath--path "B ") :to-equal '(1 2 1))
              ;; A rebuild walks the items in their new order and still
              ;; gives each one its own path back.
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "keeps the path of a node made by before or after when it is edited"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (a x y)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq a (vui-stream-open s (vui-spath--item "A")))
            (setq x (vui-stream-before a (vui-spath--item "X")))
            (setq y (vui-stream-after a (vui-spath--item "Y")))
            (vui-stream-append s (vui-spath--item "m4"))
            (vui-stream-update x (vui-spath--item "X2"))
            (vui-stream-append-to y (vui-button "more"))
            (let ((live (vui-spath--paths)))
              (expect live
                      :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                  ("X2 [copy]" "copy" (1 2 1))
                                  ("A [copy]" "copy" (1 1 1))
                                  ("Y [copy][more]" "copy" (1 3 1))
                                  ("Y [copy][more]" "more" (1 3 2))
                                  ("m4 [copy]" "copy" (1 4 1))))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "keeps the paths of a node that is not a fragment when it grows"
    ;; Growing a node makes its vnode a fragment.  A node opened with a
    ;; column or a row of widgets is kept in a fragment from the start,
    ;; so its widgets are not one level deeper after the next rebuild.
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq node (vui-stream-open
                        s (vui-vstack
                           (vui-hstack (vui-text "line A") (vui-button "copy"))
                           (vui-hstack (vui-text "line B") (vui-button "copy")))))
            (vui-stream-append-to node (vui-text " +token"))
            (vui-stream-append-to node (vui-button "copy"))
            (let ((live (vui-spath--paths)))
              (expect live
                      :to-equal
                      '(("m0 [copy]" "copy" (1 0 1))
                        ("line A [copy]" "copy" (1 1 0 0 1))
                        ("line B [copy] +token[copy]" "copy" (1 1 0 1 1))
                        ("line B [copy] +token[copy]" "copy" (1 1 2))))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "keeps the paths of a node whose vnode was replaced when it grows"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq node (vui-stream-open s (vui-text "running")))
            (vui-stream-update node (vui-hstack (vui-text "done")
                                                (vui-button "copy")))
            (vui-stream-append-to node (vui-button "copy"))
            (let ((live (vui-spath--paths)))
              (expect live
                      :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                  ("done [copy][copy]" "copy" (1 1 0 1))
                                  ("done [copy][copy]" "copy" (1 1 1))))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "does not hand the path of a removed item to a later one"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (b d)
            (vui-stream-append s (vui-spath--item "m0"))
            (setq b (vui-stream-open s (vui-spath--item "B")))
            (vui-stream-open s (vui-spath--item "C"))
            (vui-stream-remove b)       ; from the middle
            (setq d (vui-stream-open s (vui-spath--item "D")))
            (expect (vui-spath--paths)
                    :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                ("C [copy]" "copy" (1 2 1))
                                ("D [copy]" "copy" (1 3 1))))
            (vui-stream-remove d)       ; the last one
            (vui-stream-append s (vui-spath--item "E"))
            (let ((live (vui-spath--paths)))
              (expect live
                      :to-equal '(("m0 [copy]" "copy" (1 0 1))
                                  ("C [copy]" "copy" (1 2 1))
                                  ("E [copy]" "copy" (1 4 1))))
              (vui-spath--rebuild inst s)
              (expect (vui-spath--paths) :to-equal live)))
        (vui-spath--kill))))

  (it "finds the neighbour of an item that is gone"
    ;; What point falls back to when its item is not there any more: the
    ;; item next to it in the stream, not the first widget of the buffer.
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-head s)))
      (unwind-protect
          (let (gone)
            (dotimes (i 3)
              (vui-stream-append s (vui-spath--item (format "m%d" i))))
            (setq gone (vui-stream-open s (vui-spath--item "gone")))
            (dotimes (i 3)
              (vui-stream-append s (vui-spath--item (format "m%d" (+ 4 i)))))
            (vui-stream-remove gone)
            (with-current-buffer vui-spath--buffer
              (expect (vui--path-nearest-widget '(1 3 1))
                      :to-equal (vui-spath--button "m4 "))
              ;; Past the end, the last item is the nearest.
              (expect (vui--path-nearest-widget '(1 9 1))
                      :to-equal (vui-spath--button "m6 ")))
            (vui-spath--rebuild inst s)
            (with-current-buffer vui-spath--buffer
              (expect (vui--path-nearest-widget '(1 3 1))
                      :to-equal (vui-spath--button "m4 "))))
        (vui-spath--kill)))))

(describe "vui-stream paths: component rows"
  (it "starts the paths of a row at the row's own step"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-spath--mount 'vui-spath-tail s)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "started"))
            (vui-stream-append s (vui-component 'vui-spath-row :id "A"))
            (vui-stream-append s (vui-component 'vui-spath-row :id "B"))
            ;; The stream is child 0 of the root's vstack.  Inside a row
            ;; the button is child 1 of the hstack, child 0 of the vstack.
            (expect (vui-spath--path "row A") :to-equal '(0 1 0 1))
            (expect (vui-spath--path "row B") :to-equal '(0 2 0 1)))
        (vui-spath--kill))))

  (it "keeps the paths of a row when the row re-renders on its own"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-spath--mount 'vui-spath-tail s)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "started"))
            (vui-stream-append s (vui-component 'vui-spath-row :id "A"))
            (vui-stream-append s (vui-component 'vui-spath-row :id "B"))
            (with-current-buffer vui-spath--buffer
              (button-activate (vui-spath--button "row B"))
              (expect (buffer-string) :to-match "row B \\[toggle\\]\n  detail"))
            (expect (vui-spath--path "row A") :to-equal '(0 1 0 1))
            (expect (vui-spath--path "row B") :to-equal '(0 2 0 1)))
        (vui-spath--kill)))))

(describe "vui-stream paths: error boundaries"
  (it "keeps a caught error inside its own item"
    ;; A boundary without an :id is told apart by its place in the tree.
    ;; With one place for all items, the items appended after a failed
    ;; one came up as their fallback, and after a rebuild of the stream
    ;; every item did.
    (let ((vui-render-delay nil)
          (s (vui-make-stream))
          (caught 0))
      (vui-defcomponent vui-spath-risky (fail)
        :render (if fail (error "Boom") (vui-text "ok")))
      (cl-flet ((item (fail)
                  (vui-error-boundary
                   :fallback (lambda (_error) (vui-text "failed"))
                   :on-error (lambda (_error) (cl-incf caught))
                   :children (list (vui-component 'vui-spath-risky
                                                  :fail fail)))))
        (let ((inst (vui-spath--mount 'vui-spath-head s)))
          (unwind-protect
              (progn
                (vui-stream-append s (item nil))
                (vui-stream-append s (item t))
                (vui-stream-append s (item nil))
                (vui-stream-append s (item nil))
                (dolist (label '("a" "b" "c"))
                  (unless (equal label "a")
                    (vui-spath--rebuild inst s label))
                  (with-current-buffer vui-spath--buffer
                    (expect (buffer-string)
                            :to-equal (format "header\nok\nfailed\nok\nok\nstatus: %s"
                                              label)))
                  (expect caught :to-equal 1)))
            (vui-spath--kill)))))))

(describe "vui-stream paths: widgets below a live stream"
  (it "keeps their paths when the render patches around the stream"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-tail s)))
      (unwind-protect
          (let ((below '(("[send] [stop a]" "send" (1 0))
                         ("[send] [stop a]" "stop a" (1 1)))))
            ;; An empty stream is rendered like any other child.
            (expect (vui-spath--paths) :to-equal below)
            (vui-stream-append s (vui-text "m0"))
            (vui-stream-append s (vui-text "m1"))
            (expect (vui-spath--paths) :to-equal below)
            ;; From here on the stream is live and its text is left
            ;; alone: only the line below is rewritten.
            (with-current-buffer vui-spath--buffer
              (let ((probe (copy-marker 2)))
                (vui-update inst (list :stream s :label "b"))
                (expect (marker-position probe) :to-equal 2)
                (set-marker probe nil)))
            (expect (vui-spath--paths)
                    :to-equal '(("[send] [stop b]" "send" (1 0))
                                ("[send] [stop b]" "stop b" (1 1)))))
        (vui-spath--kill))))

  (it "counts the children before the stream too"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-frag s)))
      (unwind-protect
          (progn
            (expect (vui-spath--paths) :to-equal '(("[stop a]" "stop a" (3))))
            (vui-stream-append s (vui-text "m0"))
            (vui-stream-append s (vui-text "m1"))
            (with-current-buffer vui-spath--buffer
              (let ((probe (copy-marker 2)))
                (vui-update inst (list :stream s :label "b"))
                (expect (buffer-string) :to-equal "m0\nm1\n[stop b]")
                (expect (marker-position probe) :to-equal 2)
                (set-marker probe nil)))
            (expect (vui-spath--paths) :to-equal '(("[stop b]" "stop b" (3)))))
        (vui-spath--kill))))

  (it "keeps the caught error of a boundary below the stream"
    ;; A boundary without an :id is told apart by its place in the tree.
    ;; When that place changed with the first patched render, the error
    ;; was forgotten and the failing child ran once more.
    (let ((vui-render-delay nil)
          (s (vui-make-stream))
          (caught 0))
      (vui-defcomponent vui-spath-broken ()
        :render (error "Boom"))
      (vui-defcomponent vui-spath-guarded (stream label)
        :render (vui-vstack
                 (vui-stream stream)
                 (vui-error-boundary
                  :fallback (lambda (_error)
                              (vui-text (format "failed %s" label)))
                  :on-error (lambda (_error) (cl-incf caught))
                  :children (list (vui-component 'vui-spath-broken)))))
      (let ((inst (vui-spath--mount 'vui-spath-guarded s)))
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "m0"))
              (vui-stream-append s (vui-text "m1"))
              (expect caught :to-equal 1)
              (with-current-buffer vui-spath--buffer
                (let ((probe (copy-marker 2)))
                  (vui-update inst (list :stream s :label "b"))
                  (expect (buffer-string) :to-equal "m0\nm1\nfailed b")
                  ;; The render patched around the stream.
                  (expect (marker-position probe) :to-equal 2)
                  (set-marker probe nil)))
              (expect caught :to-equal 1))
          (vui-spath--kill))))))

(provide 'vui-stream-path-test)
;;; vui-stream-path-test.el ends here

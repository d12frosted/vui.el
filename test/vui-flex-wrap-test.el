;;; vui-flex-wrap-test.el --- Wrapping flex tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for `vui-flex' :wrap (issue #134): children partition into
;; rows at minimum widths in source order, rows allocate width through
;; the pure core in vui-layout.el, single-line rows render inline
;; (preserving component and widget identity exactly like a non-wrapped
;; flex), and rows containing multi-line blocks are composed as text.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-flex-wrap-test--render (vnode)
  "Render VNODE and return the buffer text without properties."
  (with-temp-buffer
    (vui-render vnode)
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "vui-flex :wrap"
  (it "wraps children into rows when their widths stop fitting"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 7 :wrap t
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb\nccc"))

  (it "keeps everything on one row while it fits"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 11 :wrap t
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb ccc"))

  (it "does not wrap without the flag"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 7
               (vui-text "aaa") (vui-text "bbb") (vui-text "ccc")))
            :to-equal "aaa bbb ccc"))

  (it "grows a flex-item into its row's leftover width"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aa")
               (vui-flex-item :grow 1 (vui-text "b"))
               (vui-text "cccccccc")))
            :to-equal "aa b      \ncccccccc"))

  (it "renders a function grower at its assigned width"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aaaa")
               (vui-flex-item :grow 1
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaa bbbbb"))

  (it "keeps a function child at :min-width on its own row when squeezed"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 8 :wrap t
               (vui-text "aaaaaa")
               (vui-flex-item :grow 0 :min-width 3
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaaaa\nbbb"))

  (it "shares a row with a function child at :min-width when it fits"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-text "aaaaaa")
               (vui-flex-item :grow 0 :min-width 3
                 (lambda (w) (vui-text (make-string w ?b))))))
            :to-equal "aaaaaa bbb"))

  (it "never shrinks a static child below its content"
    ;; a static block cannot re-render narrower, so packing the row at
    ;; :min-width would only overflow it: the children wrap instead
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 16 :wrap t
               (vui-flex-item :grow 0 :min-width 4 (vui-text "first-child"))
               (vui-flex-item :grow 0 :min-width 4 (vui-text "second-child"))))
            :to-equal "first-child\nsecond-child"))

  (it "raises a static child to a :min-width above its content"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 30 :wrap t
               (vui-flex-item :grow 0 :min-width 6 (vui-text "abc"))
               (vui-text "xyz")))
            :to-equal "abc    xyz"))

  (it "composes a row containing a multi-line block"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 9 :wrap t
               (vui-vstack (vui-text "a") (vui-text "bb"))
               (vui-text "ccc")))
            :to-equal "a  ccc\nbb    "))

  (it "pads a grower's lines to its share in a composed row"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 10 :wrap t
               (vui-vstack (vui-text "a") (vui-text "bb"))
               (vui-flex-item :grow 1 (vui-text "c"))))
            :to-equal "a  c      \nbb        "))

  (it "indents continuation rows by :indent"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 8 :indent 2 :wrap t
               (vui-text "aaa") (vui-text "bbb")))
            :to-equal "aaa\n  bbb"))

  (it "composes a row whose function child renders multi-line"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 20 :wrap t
               (vui-text "AA")
               (vui-flex-item :grow 1 :min-width 8
                 (lambda (_w) (vui-vstack (vui-text "f1") (vui-text "f2"))))
               (vui-text "ZZ")))
            :to-equal (concat "AA f1             ZZ\n"
                              "   f2               ")))

  (it "drops a child that renders nothing, like a non-wrapped flex"
    (expect (vui-flex-wrap-test--render
             (vui-flex :width 20 :wrap t
               (vui-text "aa")
               (vui-text "")
               (vui-vstack (vui-text "m1") (vui-text "m2"))))
            :to-equal "aa m1\n   m2"))

  (it "measures composed content against the buffer's fill-column"
    (with-temp-buffer
      ;; vui-mode (enabled by the first render) kills buffer-locals,
      ;; so set fill-column after a priming render.
      (vui-render (vui-text ""))
      (setq-local fill-column 12)
      (vui-render (vui-flex :width 30 :wrap t
                    (vui-vstack (vui-text "p") (vui-text "q"))
                    (vui-flex :width 'fill-column :justify :end
                      (vui-text "R"))))
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal (concat "p            R\n"
                                "q             "))))

  (it "keeps buttons clickable on wrapped rows"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-flex :width 4 :wrap t
                      (vui-text "aaaa")
                      (vui-button "Go" :on-click (lambda () (setq clicked t)))))
        (goto-char (point-min))
        (search-forward "[Go]")
        (button-activate (button-at (match-beginning 0)))
        (expect clicked :to-be t)))))

;; A stateful child for identity checks: "[xx]" (4) becomes
;; "[xxxxxxxx]" (10) when clicked.
(vui-defcomponent vui-flex-wrap-test-child (label)
  :state ((wide nil))
  :render (vui-button (if wide
                          (concat label label label label)
                        label)
            :on-click (lambda () (vui-set-state :wide t))))

(vui-defcomponent vui-flex-wrap-test-root ()
  :render (vui-flex :width 16 :wrap t
            (vui-component 'vui-flex-wrap-test-child :label "xx")
            (vui-flex-item :grow 1 (vui-text "R"))))

(describe "vui-flex :wrap identity"
  (it "keeps component state and measures it on inline rows"
    (let ((vui-render-delay nil))
      (let ((root (vui-mount (vui-component 'vui-flex-wrap-test-root)
                             "*wrap-identity*")))
        (unwind-protect
            (with-current-buffer "*wrap-identity*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "[xx] R          ")
              (let ((child (car (vui-instance-children root))))
                (goto-char (point-min))
                (search-forward "[xx")
                (button-activate (button-at (match-beginning 0)))
                ;; Re-laid out from the child's current width, same
                ;; instance, state intact.
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal "[xxxxxxxx] R    ")
                (expect (car (vui-instance-children root)) :to-be child)
                (expect (plist-get (vui-instance-state child) :wide)
                        :to-be t)))
          (kill-buffer "*wrap-identity*"))))))

;; Every widget records its place in the tree as `:vui-path', and a
;; re-render finds the widget under point again by that path.  A child
;; of the flex is one step of it, its child index as in a non-wrapping
;; flex, whichever way its row is rendered: in place, or composed from
;; the lines of the measure pass.

(defun vui-flex-wrap-test--paths ()
  "Return (LABEL PATH) for every widget of the current buffer, in order."
  (mapcar (lambda (widget)
            (list (vui--elt-get widget :vui-tag)
                  (vui--elt-get widget :vui-path)))
          (vui--collect-widgets)))

(defun vui-flex-wrap-test--goto-nth (text n)
  "Move point to the start of the Nth occurrence of TEXT in the buffer."
  (goto-char (point-min))
  (search-forward text nil nil n)
  (goto-char (match-beginning 0)))

;; Two panels side by side.  Each one is two lines tall, so the row is
;; composed, and both have the same button in the same place.
(vui-defcomponent vui-flex-wrap-test-panels ()
  :render (vui-vstack
           (vui-text "header")
           (vui-flex :wrap t :width 60
             (vui-vstack (vui-button "copy") (vui-button "copy"))
             (vui-vstack (vui-button "copy") (vui-button "send")))))

;; A row of three children.  The last one is one line tall or two,
;; which makes the row render in place or composed.
(vui-defcomponent vui-flex-wrap-test-switch (tall)
  :render (vui-vstack
           (vui-text "header")
           (vui-flex :wrap t :width 60
             (vui-button "copy")
             (vui-button "copy")
             (vui-vstack (vui-text "a") (when tall (vui-text "b"))))))

;; The first child renders nothing until SHOW is set.
(vui-defcomponent vui-flex-wrap-test-empty (show)
  :render (vui-flex :wrap t :width 60
            (vui-text (if show "x" ""))
            (vui-button "copy")
            (vui-button "copy")))

(describe "vui-flex :wrap render paths"
  (it "makes every child of a composed row a step in the path"
    (with-temp-buffer
      (vui-render (vui-flex :wrap t :width 60
                    (vui-vstack (vui-button "copy") (vui-button "copy"))
                    (vui-vstack (vui-button "copy") (vui-button "send"))))
      ;; In buffer order: the first line of both children, then the
      ;; second.
      (expect (vui-flex-wrap-test--paths)
              :to-equal '(("copy" (0 0)) ("copy" (1 0))
                          ("copy" (0 1)) ("send" (1 1))))))

  (it "numbers the children of every row by their child index"
    (with-temp-buffer
      ;; The first row renders in place, the second one is composed.
      (vui-render (vui-flex :wrap t :width 7
                    (vui-button "a")
                    (vui-button "b")
                    (vui-vstack (vui-button "c") (vui-text "x"))
                    (vui-button "d")))
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "[a] [b]\n[c] [d]\nx      ")
      (expect (vui-flex-wrap-test--paths)
              :to-equal '(("a" (0)) ("b" (1)) ("c" (2 0)) ("d" (3))))))

  (it "counts a child that renders nothing, in a row rendered in place"
    (with-temp-buffer
      (vui-render (vui-flex :wrap t :width 60
                    (vui-text "")
                    (vui-button "a")
                    (vui-button "b")))
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "[a] [b]")
      (expect (vui-flex-wrap-test--paths)
              :to-equal '(("a" (1)) ("b" (2))))))

  (it "counts a child that renders nothing, in a composed row"
    (with-temp-buffer
      (vui-render (vui-flex :wrap t :width 60
                    (vui-text "")
                    (vui-vstack (vui-button "a") (vui-text "x"))
                    (vui-button "b")))
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "[a] [b]\nx      ")
      (expect (vui-flex-wrap-test--paths)
              :to-equal '(("a" (1 0)) ("b" (2))))))

  (it "gives its children the paths of a non-wrapping flex"
    (let ((paths
           (lambda (wrap)
             (with-temp-buffer
               (vui-render
                (vui-flex :wrap wrap :width 60
                  (vui-text "")
                  (vui-button "a")
                  (vui-flex-item :grow 1 (vui-button "b"))
                  (vui-flex-item :grow 1 (lambda (_w) (vui-button "c")))))
               (vui-flex-wrap-test--paths)))))
      (expect (funcall paths nil)
              :to-equal '(("a" (1)) ("b" (2)) ("c" (3))))
      (expect (funcall paths t) :to-equal (funcall paths nil))))

  (it "gives a widget one path, composed or in place"
    (dolist (tall '(nil t))
      (with-temp-buffer
        (vui-render (vui-flex :wrap t :width 60
                      (vui-button "copy")
                      (vui-button "copy")
                      (vui-vstack (vui-text "a") (when tall (vui-text "b")))))
        ;; Two lines of text: the row was composed
        (expect (count-lines (point-min) (point-max))
                :to-equal (if tall 2 1))
        (expect (vui-flex-wrap-test--paths)
                :to-equal '(("copy" (0)) ("copy" (1)))))))

  (it "numbers function children the same way"
    ;; A function child is measured after the widths are allocated.
    (dolist (tall '(nil t))
      (with-temp-buffer
        (let ((child (lambda (label)
                       (vui-flex-item :grow 1
                         (lambda (_w)
                           (if tall
                               (vui-vstack (vui-button "copy")
                                           (vui-button label))
                             (vui-button label)))))))
          (vui-render (vui-flex :wrap t :width 60
                        (vui-text "")
                        (funcall child "copy")
                        (funcall child "send")))
          (expect (vui-flex-wrap-test--paths)
                  :to-equal (if tall
                                '(("copy" (1 0)) ("copy" (2 0))
                                  ("copy" (1 1)) ("send" (2 1)))
                              '(("copy" (1)) ("send" (2)))))))))

  (it "keeps the items of a stream apart from the widgets of other children"
    (let ((stream (vui-make-stream)))
      (vui-stream-append stream (vui-button "copy"))
      (vui-stream-append stream (vui-button "copy"))
      (with-temp-buffer
        (vui-render (vui-flex :wrap t :width 60
                      (vui-vstack (vui-button "copy") (vui-button "copy"))
                      (vui-stream stream)))
        ;; An item adds its id as a step after the path of its stream,
        ;; and the stream is child 1.
        (expect (vui-flex-wrap-test--paths)
                :to-equal '(("copy" (0 0)) ("copy" (1 0))
                            ("copy" (0 1)) ("copy" (1 1)))))))

  (it "keeps point on a button of the second child across a re-render"
    (let* ((vui-render-delay nil)
           (root (vui-mount (vui-component 'vui-flex-wrap-test-panels)
                            "*wrap-paths*")))
      (unwind-protect
          (with-current-buffer "*wrap-paths*"
            ;; The second [copy] of the buffer is the first button of
            ;; the second child.
            (vui-flex-wrap-test--goto-nth "[copy]" 2)
            (expect (current-column) :to-equal 7)
            (let ((pos (point))
                  (probe (copy-marker (point))))
              (vui-rerender root)
              ;; The text under point was rewritten: a marker in it was
              ;; thrown to the top.
              (expect (marker-position probe) :to-equal (point-min))
              (expect (point) :to-equal pos)
              (set-marker probe nil)))
        (kill-buffer "*wrap-paths*"))))

  (it "keeps point on its button when the row switches between the two ways"
    (let* ((vui-render-delay nil)
           (root (vui-mount (vui-component 'vui-flex-wrap-test-switch)
                            "*wrap-paths*")))
      (unwind-protect
          (with-current-buffer "*wrap-paths*"
            (vui-flex-wrap-test--goto-nth "[copy]" 2)
            (let ((pos (point)))
              (expect (vui-flex-wrap-test--paths)
                      :to-equal '(("copy" (1 0)) ("copy" (1 1))))
              ;; In place to composed
              (vui-update root '(:tall t))
              (expect (count-lines (point-min) (point-max)) :to-equal 3)
              (expect (vui-flex-wrap-test--paths)
                      :to-equal '(("copy" (1 0)) ("copy" (1 1))))
              (expect (point) :to-equal pos)
              ;; And back
              (vui-update root '(:tall nil))
              (expect (count-lines (point-min) (point-max)) :to-equal 2)
              (expect (point) :to-equal pos)))
        (kill-buffer "*wrap-paths*"))))

  (it "keeps point on its button when a child before it starts to render"
    (let* ((vui-render-delay nil)
           (root (vui-mount (vui-component 'vui-flex-wrap-test-empty)
                            "*wrap-paths*")))
      (unwind-protect
          (with-current-buffer "*wrap-paths*"
            (vui-flex-wrap-test--goto-nth "[copy]" 2)
            (expect (current-column) :to-equal 7)
            (vui-update root '(:show t))
            (expect (buffer-substring-no-properties (point-min) (point-max))
                    :to-equal "x [copy] [copy]")
            ;; Still on the second button
            (expect (current-column) :to-equal 9))
        (kill-buffer "*wrap-paths*")))))

(provide 'vui-flex-wrap-test)

;;; vui-flex-wrap-test.el ends here

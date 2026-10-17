;;; vui-incremental-test.el --- Incremental rendering tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for the experimental incremental renderer (issue #82), gated by
;; `vui-incremental-render'.  They assert (a) parity: an incremental
;; re-render produces the same buffer as a full rebuild, and (b) that it
;; actually patches in place (an overlay on an unchanged segment keeps
;; its exact bounds, which a full erase+rebuild cannot do), and (c) that
;; ineligible trees fall back to the full rebuild correctly.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(defun vui-inc--string (vnode)
  "Return the wholesale-rendered string of VNODE."
  (with-temp-buffer (vui--render-vnode vnode) (buffer-string)))

(defun vui-inc--paths ()
  "Return the render path of every widget in the current buffer, in order."
  (mapcar (lambda (widget) (vui--elt-get widget :vui-path))
          (vui--collect-widgets)))

(defun vui-inc--tag-at-point ()
  "Return the label of the widget at point, or nil when there is none."
  (when-let* ((elt (vui--elt-at (point))))
    (vui--elt-get elt :vui-tag)))

(describe "incremental rendering parity"
  (it "matches wholesale output across update / append / truncate"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-parity (items)
        :render (apply #'vui-vstack (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'inc-parity :items '("a" "b" "c"))
                             "*inc-par*")))
        (unwind-protect
            (with-current-buffer "*inc-par*"
              (dolist (items '(("a" "b" "c")        ; establish record
                               ("a" "B" "c")        ; mid change
                               ("a" "B" "c" "d")    ; append
                               ("a" "B")            ; truncate
                               ("a" "X" "B")        ; mid insert (shifts)
                               ("p" "q" "r" "s")))  ; full change
                (vui-update inst (list :items items))
                (expect (buffer-string)
                        :to-equal
                        (vui-inc--string
                         (apply #'vui-vstack (mapcar #'vui-text items))))))
          (kill-buffer "*inc-par*")))))

  (it "matches wholesale for a fragment of text"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-frag (items)
        :render (apply #'vui-fragment (mapcar #'vui-text items)))
      (let ((inst (vui-mount (vui-component 'inc-frag :items '("x" "y"))
                             "*inc-frag*")))
        (unwind-protect
            (with-current-buffer "*inc-frag*"
              (vui-update inst '(:items ("x" "y")))
              (vui-update inst '(:items ("x" "Y" "z")))
              (expect (buffer-string)
                      :to-equal
                      (vui-inc--string
                       (apply #'vui-fragment
                              (mapcar #'vui-text '("x" "Y" "z"))))))
          (kill-buffer "*inc-frag*"))))))

(describe "incremental rendering actually patches in place"
  (it "leaves an unchanged segment untouched (overlay keeps exact bounds)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-skip (gen)
        :render (vui-vstack
                 (vui-text "alpha")
                 (vui-text (format "beta-%d" gen))))
      (let ((inst (vui-mount (vui-component 'inc-skip :gen 0) "*inc-skip*")))
        (unwind-protect
            (with-current-buffer "*inc-skip*"
              ;; First re-render establishes the record (erases once).
              (vui-update inst '(:gen 1))
              ;; Overlay over the unchanged first line "alpha" (1..6).
              (let ((ov (make-overlay 1 6)))
                ;; Only the beta line changes; alpha must be skipped.
                (vui-update inst '(:gen 2))
                (expect (buffer-string) :to-equal "alpha\nbeta-2")
                ;; A full erase+rebuild would collapse the overlay to 1,1.
                ;; Incremental leaves alpha's text (and overlay) untouched.
                (expect (overlay-start ov) :to-equal 1)
                (expect (overlay-end ov) :to-equal 6)))
          (kill-buffer "*inc-skip*")))))

  (it "patches a segment whose string changed only in text properties"
    ;; `equal' ignores string text properties, so a segment whose
    ;; characters are unchanged but whose face (carried on the content
    ;; string) flipped must still be re-rendered, not skipped.
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-propchange (danger)
        :render (vui-vstack
                 (vui-text "header")
                 (vui-text (propertize "gauge" 'face
                                       (if danger 'error 'success)))
                 (vui-text "footer")))
      (let ((inst (vui-mount (vui-component 'inc-propchange :danger nil)
                             "*inc-props*")))
        (unwind-protect
            (with-current-buffer "*inc-props*"
              ;; First re-render establishes the content record.
              (vui-update inst '(:danger nil))
              ;; Position 8 is the first char of the gauge line.
              (expect (get-text-property 8 'face) :to-equal 'success)
              ;; Same characters, different face: must not be skipped.
              (vui-update inst '(:danger t))
              (expect (get-text-property 8 'face) :to-equal 'error)
              (expect (equal-including-properties
                       (buffer-string)
                       (vui-inc--string
                        (vui-vstack
                         (vui-text "header")
                         (vui-text (propertize "gauge" 'face 'error))
                         (vui-text "footer"))))
                      :to-be-truthy))
          (kill-buffer "*inc-props*"))))))

(describe "vui--vnode-equal"
  (it "handles a long flat children list without exhausting the stack"
    ;; Recursion must only be as deep as the tree NESTING, not the list
    ;; length: a single segment holding thousands of children (the
    ;; 10k-row viewport scenario) must not blow `max-lisp-eval-depth'.
    (let ((make (lambda ()
                  (apply #'vui-vstack
                         (mapcar (lambda (i) (vui-text (format "row-%d" i)))
                                 (number-sequence 1 5000))))))
      (expect (vui--vnode-equal (funcall make) (funcall make))
              :to-be-truthy)))

  (it "compares dotted tails with the same string-aware equality"
    (expect (vui--vnode-equal (cons "a" "b") (cons "a" "b")) :to-be-truthy)
    (expect (vui--vnode-equal (cons "a" (propertize "b" 'face 'error))
                              (cons "a" "b"))
            :to-be nil)
    (expect (vui--vnode-equal (list "a" "b") (cons "a" "b")) :to-be nil)))

(describe "incremental rendering whole-tree skip"
  (it "skips entirely when should-update returns nil"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-static ()
        :should-update nil
        :state ((tick 0))
        :render (vui-vstack (vui-text "aaa") (vui-text "bbb")))
      (let ((inst (vui-mount (vui-component 'inc-static) "*inc-static*")))
        (unwind-protect
            (with-current-buffer "*inc-static*"
              ;; First re-render establishes the record.
              (let ((vui--current-instance inst)) (vui-set-state :tick 1))
              ;; Overlay over the first line; many no-op re-renders must
              ;; leave the buffer and the overlay completely untouched.
              (let ((ov (make-overlay 1 4)))
                (dotimes (k 5)
                  (let ((vui--current-instance inst)) (vui-set-state :tick (+ 2 k))))
                (expect (buffer-string) :to-equal "aaa\nbbb")
                (expect (overlay-start ov) :to-equal 1)
                (expect (overlay-end ov) :to-equal 4)))
          (kill-buffer "*inc-static*"))))))

(describe "whole-tree eq-skip is on by default (flag off)"
  (it "skips the rebuild when the vtree is unchanged, even with the flag off"
    ;; eq-skip needs no bookkeeping and only ever skips provably-unchanged
    ;; work, so it is not gated by `vui-incremental-render'.  An overlay on
    ;; an unchanged buffer must survive no-op re-renders that would
    ;; otherwise erase and rebuild it.
    (let ((vui-incremental-render nil)
          (vui-render-delay nil))
      (vui-defcomponent eqskip-default ()
        :should-update nil
        :state ((tick 0))
        :render (vui-vstack (vui-text "xxx") (vui-text "yyy")))
      (let ((inst (vui-mount (vui-component 'eqskip-default) "*eqskip*")))
        (unwind-protect
            (with-current-buffer "*eqskip*"
              ;; First re-render establishes the record.
              (let ((vui--current-instance inst)) (vui-set-state :tick 1))
              (let ((ov (make-overlay 1 4)))
                (dotimes (k 4)
                  (let ((vui--current-instance inst)) (vui-set-state :tick (+ 2 k))))
                (expect (buffer-string) :to-equal "xxx\nyyy")
                ;; A full erase+rebuild would collapse the overlay to 1,1.
                (expect (overlay-start ov) :to-equal 1)
                (expect (overlay-end ov) :to-equal 4)))
          (kill-buffer "*eqskip*"))))))

(describe "whole-tree eq-skip keeps the root's component children"
  ;; A root that gives the same tree object as last time (its
  ;; should-update says skip, or its render returns a memoized tree)
  ;; reaches the eq-skip, which renders nothing, so no child is
  ;; reconciled.  The children are still on screen: they must stay
  ;; mounted and in the root's children list, so the next real render
  ;; reuses them.
  (dolist (flag '(nil t))
    (it (format "keeps them mounted and reuses them (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (log nil))
        (vui-defcomponent eqskip-kid (name)
          :on-unmount (push (format "unmount %s" name) log)
          :render (vui-text (format "kid %s" name)))
        (vui-defcomponent eqskip-root (tick)
          ;; renders again only for an even tick
          :should-update (= 0 (% (plist-get props :tick) 2))
          :render (vui-vstack
                   (vui-component 'eqskip-kid :name (format "a%s" tick))
                   (vui-component 'eqskip-kid :name (format "b%s" tick))))
        (let ((inst (vui-mount (vui-component 'eqskip-root :tick 0)
                               "*eqskip-kids*")))
          (unwind-protect
              (with-current-buffer "*eqskip-kids*"
                ;; First re-render establishes the record (with the flag
                ;; on, a component-list record).
                (vui-update inst '(:tick 2))
                (let ((kids (vui-instance-children inst)))
                  (expect (length kids) :to-equal 2)
                  ;; odd tick: the root skips, the commit renders nothing
                  (vui-update inst '(:tick 3))
                  (expect (buffer-string) :to-equal "kid a2\nkid b2")
                  (expect (length (vui-instance-children inst)) :to-equal 2)
                  (expect (cl-mapcar #'eq (vui-instance-children inst) kids)
                          :to-equal '(t t))
                  (expect log :to-equal nil)
                  ;; the next real render reuses the same instances (with
                  ;; the flag on it patches them in place)
                  (vui-update inst '(:tick 4))
                  (expect (buffer-string) :to-equal "kid a4\nkid b4")
                  (expect (length (vui-instance-children inst)) :to-equal 2)
                  (expect (cl-mapcar #'eq (vui-instance-children inst) kids)
                          :to-equal '(t t))
                  (expect log :to-equal nil)))
            (kill-buffer "*eqskip-kids*"))))))

  (dolist (flag '(nil t))
    (it (format "keeps them when the root's render returns the same tree (flag %s)"
                (if flag "on" "off"))
      ;; Here the root does render, but `vui-use-memo' gives back the
      ;; tree it returned last time, so the commit reaches the same skip.
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (renders 0)
            (log nil))
        (vui-defcomponent eqskip-memo-kid (name)
          :on-unmount (push (format "unmount %s" name) log)
          :render (vui-text (format "kid %s" name)))
        (vui-defcomponent eqskip-memo-root (label)
          :state ((tick 0))
          :render (progn
                    (setq renders (1+ renders))
                    (vui-use-memo (label)
                      (vui-vstack
                       (vui-component 'eqskip-memo-kid :name (format "a-%s" label))
                       (vui-component 'eqskip-memo-kid :name (format "b-%s" label))))))
        (let ((inst (vui-mount (vui-component 'eqskip-memo-root :label "x")
                               "*eqskip-memo*")))
          (unwind-protect
              (with-current-buffer "*eqskip-memo*"
                ;; First re-render establishes the record.
                (let ((vui--current-instance inst)) (vui-set-state :tick 1))
                (let ((kids (vui-instance-children inst))
                      (tree (vui-instance-cached-vtree inst)))
                  (expect (length kids) :to-equal 2)
                  (setq renders 0)
                  ;; the root renders, the memo gives back the same tree
                  (let ((vui--current-instance inst)) (vui-set-state :tick 2))
                  (expect renders :to-equal 1)
                  (expect (vui-instance-cached-vtree inst) :to-be tree)
                  (expect (buffer-string) :to-equal "kid a-x\nkid b-x")
                  (expect (length (vui-instance-children inst)) :to-equal 2)
                  (expect (cl-mapcar #'eq (vui-instance-children inst) kids)
                          :to-equal '(t t))
                  (expect log :to-equal nil)
                  ;; a new label renders the same instances again
                  (vui-update inst '(:label "y"))
                  (expect (buffer-string) :to-equal "kid a-y\nkid b-y")
                  (expect (length (vui-instance-children inst)) :to-equal 2)
                  (expect (cl-mapcar #'eq (vui-instance-children inst) kids)
                          :to-equal '(t t))
                  (expect log :to-equal nil)))
            (kill-buffer "*eqskip-memo*"))))))

  (it "keeps the contexts recorded for the subtree"
    ;; The record is the root's own reads plus each child's.  A skipped
    ;; root reads nothing this time, and its children did not render, so
    ;; the record from the last real render still describes the buffer.
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcontext eqskip-root-ctx 'root)
      (vui-defcontext eqskip-kid-ctx 'kid)
      (vui-defcomponent eqskip-ctx-kid ()
        :render (vui-text (format "%s" (vui-use-context eqskip-kid-ctx-context))))
      (vui-defcomponent eqskip-ctx-root (tick)
        :should-update (= 0 (% (plist-get props :tick) 2))
        :render (vui-vstack
                 (vui-text (format "%s %s" tick
                                   (vui-use-context eqskip-root-ctx-context)))
                 (vui-component 'eqskip-ctx-kid)))
      (let ((inst (vui-mount (vui-component 'eqskip-ctx-root :tick 0)
                             "*eqskip-ctx*")))
        (unwind-protect
            (with-current-buffer "*eqskip-ctx*"
              (vui-update inst '(:tick 2))
              (let ((recorded (vui-instance-consumed-contexts inst)))
                (expect (length recorded) :to-equal 2)
                (vui-update inst '(:tick 3))
                (expect (buffer-string) :to-equal "2 root\nkid")
                (expect (vui-instance-consumed-contexts inst)
                        :to-equal recorded)))
          (kill-buffer "*eqskip-ctx*"))))))

(describe "incremental component-list patching"
  ;; A keyed list of child components with should-update: unchanged
  ;; children bail out of re-rendering and keep their buffer regions.
  (it "matches wholesale across update / append / delete / reorder"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-child (id label)
        :should-update (not (equal label (plist-get prev-props :label)))
        :render (vui-text (format "[%s:%s]" id label)))
      (vui-defcomponent inc-cl (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-cl-child
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-cl
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-cl*")))
        (unwind-protect
            (with-current-buffer "*inc-cl*"
              (expect (buffer-string) :to-equal "[1:a]\n[2:b]\n[3:c]")
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c")))) ; update
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]")
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c") (4 . "d")))) ; append
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]\n[4:d]")
              (vui-update inst '(:items ((1 . "a") (3 . "c") (4 . "d")))) ; delete middle
              (expect (buffer-string) :to-equal "[1:a]\n[3:c]\n[4:d]")
              (vui-update inst '(:items ((4 . "d") (1 . "a") (3 . "c")))) ; reorder
              (expect (buffer-string) :to-equal "[4:d]\n[1:a]\n[3:c]"))
          (kill-buffer "*inc-cl*")))))

  (it "skips the children that bail out (re-renders only what changed)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-cl-count (id label)
        :should-update (not (equal label (plist-get prev-props :label)))
        :render (progn (setq renders (1+ renders)) (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-cl2 (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-cl-count
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-cl2
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-cl2*")))
        (unwind-protect
            (with-current-buffer "*inc-cl2*"
              (setq renders 0)
              ;; change only item 2: only it should re-render
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 1)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]"))
          (kill-buffer "*inc-cl2*")))))

  (it "preserves child state across a reorder"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-stateful (id)
        :state ((clicks 0))
        :render (vui-text (format "[%s:%d]" id clicks)))
      (vui-defcomponent inc-cl3 (order)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id) (vui-component 'inc-cl-stateful :key id :id id))
                               order)))
      (let ((inst (vui-mount (vui-component 'inc-cl3 :order '("a" "b" "c"))
                             "*inc-cl3*")))
        (unwind-protect
            (with-current-buffer "*inc-cl3*"
              (let ((a (cl-find "a" (vui-get-component-instances 'inc-cl-stateful inst)
                                :key (lambda (i) (plist-get (vui-instance-props i) :id))
                                :test #'equal)))
                (let ((vui--current-instance a)) (vui-set-state :clicks 5)))
              (expect (buffer-string) :to-equal "[a:5]\n[b:0]\n[c:0]")
              (vui-update inst '(:order ("c" "b" "a")))
              ;; a kept its state and moved to the end
              (expect (buffer-string) :to-equal "[c:0]\n[b:0]\n[a:5]"))
          (kill-buffer "*inc-cl3*")))))

  (it "keeps point and a caught error on a patched component"
    ;; A widget and a `vui-error-boundary' without :id are found again by
    ;; their render path, so a patched component has to render under the
    ;; step its container gives it in a wholesale render.
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (errors nil))
      (vui-defcomponent inc-cl-card (name fail)
        :render (vui-hstack
                 (vui-text (format "card %s" name))
                 (vui-button "open")
                 (when fail
                   (vui-error-boundary
                    :fallback (lambda (_e) (vui-text "FALLBACK"))
                    :on-error (lambda (_e) (push name errors))
                    :children (list 'boom)))))
      (vui-defcomponent inc-cl-cards (tick)
        :render (vui-vstack
                 (vui-component 'inc-cl-card :name (format "A%s" tick))
                 (vui-component 'inc-cl-card :name (format "B%s" tick) :fail t)
                 (vui-component 'inc-cl-card :name (format "C%s" tick))))
      (let ((inst (vui-mount (vui-component 'inc-cl-cards :tick 0)
                             "*inc-cl-path*")))
        (unwind-protect
            (with-current-buffer "*inc-cl-path*"
              ;; Point on the [open] button of card C.
              (goto-char (point-max))
              (search-backward "[open]")
              ;; The first re-render is wholesale and records the list,
              ;; the second one patches it.
              (vui-update inst (list :tick 1))
              (expect (plist-get (vui-instance-render-record inst) :kind)
                      :to-be 'components)
              (vui-update inst (list :tick 2))
              (expect (buffer-string)
                      :to-equal (concat "card A2 [open]\n"
                                        "card B2 [open] FALLBACK\n"
                                        "card C2 [open]"))
              (expect (vui-inc--tag-at-point) :to-equal "open")
              (expect (line-number-at-pos) :to-equal 3)
              ;; The boundary of card B still holds its error.
              (expect errors :to-equal '("B0")))
          (kill-buffer "*inc-cl-path*")))))

  (it "gives widgets the paths of a wholesale render"
    ;; The step is the index among the container's children, nil ones
    ;; counted, and a fragment keeps its nil children.  A child that
    ;; bails out keeps the paths of its last render, so it may bail only
    ;; while its index stays the same.
    (let ((vui-render-delay nil)
          (shapes '((nil a b c)         ; mount
                    (nil a b c)         ; wholesale, records the list
                    (nil a b c)         ; patched, index is not position
                    (a nil b c)         ; a moves up, b and c stay
                    (a b nil c)         ; b moves up
                    (a b c)             ; c moves up
                    (b a c)             ; reorder by key
                    (a nil nil c)       ; b is gone
                    (a nil b c))))      ; b is back: a new instance
      (vui-defcomponent inc-cl-row (id)
        :memo t
        :render (vui-hstack (vui-text (format "row %s" id))
                            (vui-button "open")))
      (vui-defcomponent inc-cl-rows (shape)
        :render (apply #'vui-fragment
                       (mapcar (lambda (id)
                                 (and id (vui-component 'inc-cl-row
                                           :key id :id id)))
                               shape)))
      (cl-flet ((render-all (flag)
                  ;; The buffer text and widget paths after each shape.
                  (let* ((vui-incremental-render flag)
                         (inst (vui-mount (vui-component 'inc-cl-rows
                                            :shape (car shapes))
                                          "*inc-cl-steps*")))
                    (unwind-protect
                        (with-current-buffer "*inc-cl-steps*"
                          (mapcar (lambda (shape)
                                    (vui-update inst (list :shape shape))
                                    (cons (buffer-substring-no-properties
                                           (point-min) (point-max))
                                          (vui-inc--paths)))
                                  (cdr shapes)))
                      (kill-buffer "*inc-cl-steps*")))))
        (let ((wholesale (render-all nil)))
          ;; Shape (a b nil c): rows a, b and c are children 0, 1 and 3.
          (expect (cdr (nth 3 wholesale)) :to-equal '((0 1) (1 1) (3 1)))
          (expect (render-all t) :to-equal wholesale)))))

  (it "rewrites a child that bails out only when its index changed"
    ;; Counterpart of the spec above: a child whose index is the same
    ;; still bails out, and its text stays in the buffer.  A text
    ;; property put on the rows tells which of them were rewritten.
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-kept (id)
        :memo t
        :render (vui-hstack (vui-text (format "row %s" id))
                            (vui-button "open")))
      (vui-defcomponent inc-cl-kept-rows (shape)
        :render (apply #'vui-fragment
                       (mapcar (lambda (id)
                                 (and id (vui-component 'inc-cl-kept
                                           :key id :id id)))
                               shape)))
      (let ((inst (vui-mount (vui-component 'inc-cl-kept-rows
                               :shape '(nil a b))
                             "*inc-cl-kept*")))
        (unwind-protect
            (with-current-buffer "*inc-cl-kept*"
              (cl-flet ((kept-p (text)
                          (save-excursion
                            (goto-char (point-min))
                            (search-forward text)
                            (get-text-property (match-beginning 0)
                                               'inc-kept))))
                (vui-update inst (list :shape '(nil a b)))
                (with-silent-modifications
                  (put-text-property (point-min) (point-max) 'inc-kept t))
                ;; Same indexes: both rows stay.
                (vui-update inst (list :shape '(nil a b)))
                (expect (kept-p "row a") :to-be-truthy)
                (expect (kept-p "row b") :to-be-truthy)
                ;; Row a moves from index 1 to 0, row b stays at 2.
                (vui-update inst (list :shape '(a nil b)))
                (expect (kept-p "row a") :not :to-be-truthy)
                (expect (kept-p "row b") :to-be-truthy)
                (expect (vui-inc--paths) :to-equal '((0 1) (2 1)))))
          (kill-buffer "*inc-cl-kept*")))))

  (it "leaves the fields around a rewritten child alone"
    ;; A field that ends where a rewritten child starts must not take in
    ;; its text, and a field that starts where it ends must not take it
    ;; in or be forgotten, nor must its placeholder.  Rows of a fragment
    ;; have no separator between them.
    (let ((vui-render-delay nil)
          (steps '((:shape (nil a b))
                   (:shape (nil a b))                ; records the list
                   (:shape (a nil b))                ; a rewritten, b kept
                   (:shape (a b))                    ; b rewritten
                   (:shape (a b) :label "changed"))))  ; b rewritten
      (vui-defcomponent inc-cl-form (id label)
        :memo t
        :render (vui-hstack (vui-field :value "" :size 3
                                       :placeholder (format "%s?" id))
                            (vui-text (or label "row"))
                            (vui-field :value (format ">%s" id) :size 3)))
      (vui-defcomponent inc-cl-forms (shape label)
        :render (apply #'vui-fragment
                       (mapcar (lambda (id)
                                 (and id (vui-component 'inc-cl-form
                                           :key id :id id
                                           :label (and (eq id 'b) label))))
                               shape)))
      (cl-flet ((render-all (flag)
                  ;; The value and bounds of every known field after each
                  ;; step, in buffer order (a dead field has no bounds),
                  ;; and the bounds of the placeholders.
                  (let* ((vui-incremental-render flag)
                         (inst (vui-mount (vui-component 'inc-cl-forms
                                            :shape '(nil a b))
                                          "*inc-cl-forms*")))
                    (unwind-protect
                        (with-current-buffer "*inc-cl-forms*"
                          (mapcar
                           (lambda (props)
                             (vui-update inst props)
                             (list
                              (sort (mapcar (lambda (w)
                                              (list (widget-value w)
                                                    (widget-field-start w)
                                                    (widget-field-end w)))
                                            widget-field-list)
                                    (lambda (x y)
                                      (< (or (nth 1 x) 0) (or (nth 1 y) 0))))
                              (sort (mapcar (lambda (ov)
                                              (list (overlay-start ov)
                                                    (overlay-end ov)))
                                            (seq-filter
                                             (lambda (ov)
                                               (overlay-get ov 'vui-placeholder))
                                             (overlays-in (point-min)
                                                          (point-max))))
                                    #'car-less-than-car)))
                           (cdr steps)))
                      (kill-buffer "*inc-cl-forms*")))))
        (let ((wholesale (render-all nil)))
          (expect (car (last wholesale))
                  :to-equal '((("" 1 4) (">a" 9 12) ("" 12 15) (">b" 24 27))
                              ((1 4) (12 15))))
          (expect (render-all t) :to-equal wholesale)))))

  (it "keeps an empty overlay at the edge of a rewritten child empty"
    ;; An empty overlay is a mark at one position (another package's,
    ;; or the line highlight of `hl-line-mode' on an empty last line).
    ;; One where a rewritten or an appended child ends stays after it.
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-cl-mark-row (id label)
        :memo t
        :render (vui-text (format "[%s %s]" id label)))
      (vui-defcomponent inc-cl-mark-rows (ids label)
        :render (apply #'vui-fragment
                       (mapcar (lambda (id)
                                 (vui-component 'inc-cl-mark-row
                                   :key id :id id
                                   :label (if (eq id 'b) label "x")))
                               ids)))
      (let ((inst (vui-mount (vui-component 'inc-cl-mark-rows
                               :ids '(a b c) :label "x")
                             "*inc-cl-mark*")))
        (unwind-protect
            (with-current-buffer "*inc-cl-mark*"
              (vui-update inst '(:ids (a b c) :label "x"))
              (let ((mark (make-overlay 11 11))
                    (end-mark (make-overlay (point-max) (point-max))))
                (expect (buffer-substring 11 (point-max)) :to-equal "[c x]")
                ;; Row b is rewritten longer.
                (vui-update inst '(:ids (a b c) :label "longer"))
                (expect (buffer-string) :to-equal "[a x][b longer][c x]")
                (expect (list (overlay-start mark) (overlay-end mark))
                        :to-equal '(16 16))
                ;; Row d is appended after the end mark.
                (vui-update inst '(:ids (a b c d) :label "longer"))
                (expect (list (overlay-start end-mark) (overlay-end end-mark))
                        :to-equal (list (point-max) (point-max)))))
          (kill-buffer "*inc-cl-mark*"))))))

;; A vstack puts a separator only between two children that write text:
;; a child that writes nothing gets no separator.  The patcher has to
;; keep the separators where a wholesale render puts them, also before a
;; child that bails out of its render.

(vui-defcomponent inc-ce-child (label)
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-text label))

(vui-defcomponent inc-ce (items spacing)
  :render (apply #'vui-vstack
                 :spacing spacing
                 (mapcar (lambda (it)
                           (vui-component 'inc-ce-child
                             :key (car it) :label (cdr it)))
                         items)))

(defun inc-ce--wholesale (items spacing)
  "Buffer text of a fresh, wholesale render of `inc-ce' with ITEMS."
  (let ((vui-incremental-render nil))
    (vui-mount (vui-component 'inc-ce :items items :spacing spacing)
               "*inc-ce-w*")
    (prog1 (with-current-buffer "*inc-ce-w*" (buffer-string))
      (kill-buffer "*inc-ce-w*"))))

(describe "incremental component-list patching of a moved child"
  ;; A keyed child that moves up renders at its new place before its old
  ;; text is deleted, so that text has to be measured before the walk
  (dolist (case '(((("b" . "BBB") ("a" . "A") ("c" . "C")) . "BBB\nA\nC")
                  ((("b" . "") ("a" . "A") ("c" . "C")) . "A\nC")
                  ;; the moved child's old text is deleted after the list
                  ((("c" . "CCC") ("a" . "A")) . "CCC\nA")))
    (it (format "deletes the old text of a child that moved and changed length (%S)"
                (mapcar #'car (car case)))
      (let ((vui-incremental-render t)
            (vui-render-delay nil))
        (let ((inst (vui-mount (vui-component 'inc-ce
                                 :items '(("a" . "A") ("b" . "B") ("c" . "C"))
                                 :spacing 0)
                               "*inc-ce*")))
          (unwind-protect
              (progn
                (vui-rerender inst)
                (vui-update inst (list :items (car case) :spacing 0))
                (expect (plist-get (vui-instance-render-record inst) :kind)
                        :to-be 'components)
                (expect (with-current-buffer "*inc-ce*" (buffer-string))
                        :to-equal (cdr case)))
            (kill-buffer "*inc-ce*")))))))

(describe "incremental component-list patching with children that write nothing"
  (dolist (spacing '(0 1))
    (it (format "keeps the separators of a wholesale render (spacing %d)" spacing)
      (let ((vui-incremental-render t)
            (vui-render-delay nil)
            (states '((("a" . "") ("b" . "B") ("c" . "C"))
                      ;; a writes text now: b needs a separator, so it
                      ;; renders again although its label did not change;
                      ;; c bails
                      (("a" . "A") ("b" . "B") ("c" . "C"))
                      (("a" . "") ("b" . "") ("c" . "C"))
                      (("a" . "") ("b" . "") ("c" . ""))
                      (("a" . "A") ("b" . "") ("c" . "C"))
                      (("a" . "A") ("b" . ""))
                      (("b" . "B") ("a" . ""))
                      (("b" . "B") ("a" . "") ("d" . "D"))
                      (("b" . "") ("a" . "") ("d" . "D"))
                      ;; a child with text is removed after empty ones
                      (("b" . "") ("a" . ""))
                      (("b" . "B") ("a" . ""))
                      ;; an empty child is removed after one with text
                      (("b" . "B")))))
        (let ((inst (vui-mount (vui-component 'inc-ce
                                 :items (car states) :spacing spacing)
                               "*inc-ce*")))
          (unwind-protect
              (progn
                ;; The mount writes no record; this render does
                (vui-rerender inst)
                (dolist (items states)
                  (vui-update inst (list :items items :spacing spacing))
                  (expect (plist-get (vui-instance-render-record inst) :kind)
                          :to-be 'components)
                  (expect (with-current-buffer "*inc-ce*" (buffer-string))
                          :to-equal (inc-ce--wholesale items spacing))))
            (kill-buffer "*inc-ce*")))))))

;; A child that holds a `vui-stream' changes its text between renders of
;; the list: items come and go, and the stream can go empty in place.
;; The separator after such a child is what the last render wrote, and
;; a stream that starts right after text the patch writes again must not
;; take that text in.

(vui-defcomponent inc-csx-chat (stream)
  :render (vui-stream stream))

(vui-defcomponent inc-csx-chat-bail (stream)
  :should-update nil
  :render (vui-stream stream))

(vui-defcomponent inc-csx-box (tick)
  :render (vui-text (format "box %s" tick)))

(vui-defcomponent inc-csx-row (label)
  :render (vui-text label))

(vui-defcomponent inc-csx-shell (stream tick chat)
  :render (vui-vstack
           (vui-component chat :stream stream)
           (vui-component 'inc-csx-box :tick tick)))

(defmacro inc-csx--with (chat &rest body)
  "Mount `inc-csx-shell' with a CHAT child and a fresh stream S, run BODY.
INST is the root; the list is recorded (a render after the mount)
before BODY runs, while the stream is still empty."
  (declare (indent 1))
  `(let* ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream))
          (inst (vui-mount (vui-component 'inc-csx-shell
                             :stream s :tick 0 :chat ,chat)
                           "*inc-csx*")))
     (unwind-protect
         (with-current-buffer "*inc-csx*"
           (vui-rerender inst)
           ,@body)
       (kill-buffer "*inc-csx*"))))

(describe "incremental component-list patching of a stream that goes empty"
  (it "removes the separator after a stream whose only item was removed"
    (inc-csx--with 'inc-csx-chat
      (let ((node (vui-stream-open s (vui-text "a"))))
        (expect (buffer-string) :to-equal "a\nbox 0")
        (vui-stream-remove node)
        (expect (buffer-string) :to-equal "box 0")
        (vui-update inst (list :stream s :tick 1 :chat 'inc-csx-chat))
        (expect (buffer-string) :to-equal "box 1"))))

  (it "removes the separator after a stream whose last item became empty"
    (inc-csx--with 'inc-csx-chat
      (vui-stream-append s (vui-text "a"))
      (vui-stream-update-last s (vui-text ""))
      (expect (buffer-string) :to-equal "box 0")
      (vui-update inst (list :stream s :tick 1 :chat 'inc-csx-chat))
      (expect (buffer-string) :to-equal "box 1")))

  (it "shows the first item of a stream whose child bails out"
    (inc-csx--with 'inc-csx-chat-bail
      (vui-stream-append s (vui-text "a"))
      (expect (buffer-string) :to-equal "a\nbox 0")))

  (it "empties a stream whose child bails out"
    (inc-csx--with 'inc-csx-chat-bail
      (let ((node (vui-stream-open s (vui-text "a"))))
        (vui-stream-remove node)
        (expect (buffer-string) :to-equal "box 0")
        (vui-update inst (list :stream s :tick 1 :chat 'inc-csx-chat-bail))
        (expect (buffer-string) :to-equal "box 1"))))

  (it "empties a stream that had text when the list was recorded"
    (let* ((vui-incremental-render t)
           (vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-mount (vui-component 'inc-csx-shell
                              :stream s :tick 0 :chat 'inc-csx-chat)
                            "*inc-csx*")))
      (unwind-protect
          (with-current-buffer "*inc-csx*"
            (let ((node (vui-stream-open s (vui-text "a"))))
              (vui-rerender inst)
              (vui-stream-remove node)
              (expect (buffer-string) :to-equal "box 0")
              (vui-update inst (list :stream s :tick 1 :chat 'inc-csx-chat))
              (expect (buffer-string) :to-equal "box 1")))
        (kill-buffer "*inc-csx*"))))

  (it "drops the separator after a stream left with an empty row"
    ;; The remove leaves text that is all an empty row: the stream is
    ;; empty in the buffer with no render of the list
    (let* ((vui-incremental-render t)
           (vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-mount (vui-component 'inc-csx-shell
                              :stream s :tick 0 :chat 'inc-csx-chat)
                            "*inc-csx*")))
      (unwind-protect
          (with-current-buffer "*inc-csx*"
            (let ((node (vui-stream-open s (vui-text "a"))))
              (vui-rerender inst)
              (vui-stream-append s (vui-component 'inc-csx-row :label ""))
              (vui-stream-remove node)
              (vui-update inst (list :stream s :tick 1 :chat 'inc-csx-chat))
              (expect (buffer-string) :to-equal "box 1")))
        (kill-buffer "*inc-csx*")))))

(vui-defcomponent inc-csm-label (label)
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-text label))

(vui-defcomponent inc-csm-chat (stream)
  :render (vui-stream stream))

(vui-defcomponent inc-csm-chat-bail (stream)
  :should-update nil
  :render (vui-stream stream))

(vui-defcomponent inc-csm-root (label chat stream mode)
  :render (let ((kids (list (vui-component 'inc-csm-label :key "l" :label label)
                            (when chat
                              (vui-component chat :key "c" :stream stream)))))
            (if (eq mode 'fragment)
                (apply #'vui-fragment kids)
              (apply #'vui-vstack kids))))

(describe "incremental component-list patching right before a stream"
  ;; With no separator between them, text written again for the child
  ;; before the stream starts where the stream starts
  (dolist (mode '(vstack fragment))
    (it (format "lets go of a stream whose child was removed (%s)" mode)
      (let* ((vui-incremental-render t)
             (vui-render-delay nil)
             (s (vui-make-stream))
             (inst (progn
                     (vui-stream-append s (vui-text "s0"))
                     (vui-mount (vui-component 'inc-csm-root
                                  :label "" :chat 'inc-csm-chat :stream s
                                  :mode mode)
                                "*inc-csm*"))))
        (unwind-protect
            (with-current-buffer "*inc-csm*"
              (vui-rerender inst)
              ;; the label gets text, the stream child goes
              (vui-update inst (list :label "x" :chat nil :stream s :mode mode))
              (expect (buffer-string) :to-equal "x")
              ;; the stream is out of the tree: an append writes nothing
              (vui-stream-append s (vui-text "s1"))
              (expect (buffer-string) :to-equal "x"))
          (kill-buffer "*inc-csm*")))))

  (it "keeps the text of the child before a stream child that bails (fragment)"
    (let* ((vui-incremental-render t)
           (vui-render-delay nil)
           (s (vui-make-stream))
           (inst (progn
                   (vui-stream-append s (vui-text "s0"))
                   (vui-mount (vui-component 'inc-csm-root
                                :label "ab" :chat 'inc-csm-chat-bail :stream s
                                :mode 'fragment)
                              "*inc-csm*"))))
      (unwind-protect
          (with-current-buffer "*inc-csm*"
            (vui-rerender inst)
            (vui-update inst (list :label "cd" :chat 'inc-csm-chat-bail
                                   :stream s :mode 'fragment))
            (expect (buffer-string) :to-equal "cds0")
            (vui-update inst (list :label "e" :chat 'inc-csm-chat-bail
                                   :stream s :mode 'fragment))
            (expect (buffer-string) :to-equal "es0")
            (vui-stream-append s (vui-text "s1"))
            (expect (buffer-string) :to-equal "es0\ns1"))
        (kill-buffer "*inc-csm*")))))

;; Random lists of child components, patched one update after another
;; and compared after each with a fresh wholesale render: the text, and
;; the render path of every widget.  Children may write nothing, move,
;; come and go; one of them may hold a stream that gets nodes, plain
;; items and component rows between the renders, and whose nodes are
;; rewritten and removed, so the stream also goes empty and back.  The
;; text is compared after each change of the stream as well.

(defconst inc-rnd--labels '("" "" "x" "yy" "multi\nline")
  "Labels of the children; an empty one writes nothing.")

(defun inc-rnd--child (label)
  (if (string-empty-p label)
      (vui-text "")
    (vui-hstack (vui-text label) (vui-button label))))

(vui-defcomponent inc-rnd-bail (label)
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (inc-rnd--child label))

(vui-defcomponent inc-rnd-plain (label)
  :render (inc-rnd--child label))

(defun inc-rnd--chat (label stream)
  (if (string-empty-p label)
      (vui-stream stream)
    (vui-vstack (vui-text label) (vui-stream stream))))

(vui-defcomponent inc-rnd-chat-bail (label stream)
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (inc-rnd--chat label stream))

(vui-defcomponent inc-rnd-chat (label stream)
  :render (inc-rnd--chat label stream))

(vui-defcomponent inc-rnd-root (items mode spacing stream)
  :render
  (let ((kids (mapcar (lambda (it)
                        (when it
                          (pcase-let ((`(,type ,key ,label) it))
                            (vui-component type :key key :label label
                              :stream stream))))
                      items)))
    (if (eq mode 'fragment)
        (apply #'vui-fragment kids)
      (apply #'vui-vstack :spacing spacing kids))))

(defun inc-rnd--items (prev chat)
  "Random children, made from PREV: labels change, children come, go
and move.  CHAT is the type of the child that holds the stream."
  (let* ((pool (copy-sequence (remq nil prev)))
         (free (cl-set-difference '("a" "b" "c" "d" "s") (mapcar #'cadr pool)
                                  :test #'equal)))
    (setq pool (mapcar (lambda (it)
                         (if (zerop (random 3))
                             (list (car it) (cadr it)
                                   (nth (random (length inc-rnd--labels))
                                        inc-rnd--labels))
                           it))
                       pool))
    (when (and (cdr pool) (zerop (random 4)))
      (setq pool (remove (nth (random (length pool)) pool) pool)))
    (when (and free (zerop (random 3)))
      (let* ((key (nth (random (length free)) free))
             (type (cond ((equal key "s") chat)
                         ((zerop (random 2)) 'inc-rnd-bail)
                         (t 'inc-rnd-plain)))
             (pos (random (1+ (length pool)))))
        (setq pool (append (cl-subseq pool 0 pos)
                           (list (list type key (nth (random (length inc-rnd--labels))
                                                     inc-rnd--labels)))
                           (nthcdr pos pool)))))
    (when (and (cdr pool) (zerop (random 3)))
      (let* ((it (nth (random (length pool)) pool))
             (rest (remq it pool))
             (pos (random (1+ (length rest)))))
        (setq pool (append (cl-subseq rest 0 pos) (list it) (nthcdr pos rest)))))
    ;; nil children are skipped, as in any render
    (let ((out nil))
      (dolist (it pool (nreverse out))
        (when (zerop (random 6)) (push nil out))
        (push it out)))))

(defun inc-rnd--seen (buffer)
  "The text of BUFFER and the render path of each of its widgets."
  (with-current-buffer buffer
    (list (buffer-substring-no-properties (point-min) (point-max))
          (mapcar (lambda (widget) (vui--elt-get widget :vui-path))
                  (vui--collect-widgets)))))

(defun inc-rnd--wholesale (props stream)
  "What a fresh wholesale render of `inc-rnd-root' with PROPS shows.
The render gets a copy of STREAM, with the same items."
  (let* ((vui-incremental-render nil)
         (copy (vui-make-stream))
         (props (plist-put (copy-sequence props) :stream copy)))
    (dolist (item (reverse (vui-stream-handle-items-rev stream)))
      ;; a component row is a mounted instance; the copy gets its vnode
      (vui-stream-append copy (if (vui-instance-p item)
                                  (vui-instance-vnode item)
                                item)))
    (vui-mount (apply #'vui-component 'inc-rnd-root props) "*inc-rnd-w*")
    (prog1 (inc-rnd--seen "*inc-rnd-w*")
      (kill-buffer "*inc-rnd-w*"))))

(describe "incremental component-list patching, randomized"
  (it "matches a wholesale render after every patch"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (failure nil))
      (random "vui-incremental-component-list")
      (cl-block done
        (dotimes (run 40)
          (let* ((chat (if (zerop (random 2)) 'inc-rnd-chat-bail 'inc-rnd-chat))
                 (mode (if (zerop (random 3)) 'fragment 'vstack))
                 (spacing (random 2))
                 (s (vui-make-stream))
                 (nodes nil)
                 (n 0)
                 (items (inc-rnd--items (list (list chat "s" "")) chat))
                 (trace nil)
                 (inst nil))
            (unwind-protect
                (progn
                  (setq inst (vui-mount (vui-component 'inc-rnd-root
                                          :items items :mode mode
                                          :spacing spacing :stream s)
                                        "*inc-rnd*"))
                  (vui-rerender inst)
                  (cl-flet ((check (step update)
                              ;; With UPDATE the root renders the new items
                              ;; first; without, only the stream changed
                              (let* ((props (list :items items :mode mode
                                                  :spacing spacing :stream s))
                                     (err nil)
                                     (got (condition-case e
                                              (progn
                                                (when update (vui-update inst props))
                                                (inc-rnd--seen "*inc-rnd*"))
                                            (error (setq err e) nil)))
                                     (want (inc-rnd--wholesale props s)))
                                (unless (and (not err) (equal got want))
                                  (setq failure (list :run run :step step
                                                      :update update :mode mode
                                                      :spacing spacing :chat chat
                                                      :err err :got got :want want
                                                      :trace (reverse trace)))
                                  (cl-return-from done)))))
                    (dotimes (step 20)
                      ;; the stream changes between renders
                      (pcase (random 6)
                        (0 (push (vui-stream-open
                                  s (vui-text (format "m%d" (cl-incf n))))
                                 nodes)
                           (push 'open trace))
                        (1 (when nodes
                             (let ((node (nth (random (length nodes)) nodes)))
                               (vui-stream-remove node)
                               (setq nodes (delq node nodes))
                               (push 'remove trace))))
                        ;; also to nothing
                        (2 (when nodes
                             (vui-stream-update (nth (random (length nodes)) nodes)
                                                (vui-text (if (zerop (random 3))
                                                              ""
                                                            (format "u%d" step))))
                             (push 'update trace)))
                        (3 (vui-stream-append s (vui-text (format "a%d" (cl-incf n))))
                           (push 'append trace))
                        ;; a component row, which may write nothing
                        (4 (vui-stream-append
                            s (vui-component 'inc-csx-row
                                :label (if (zerop (random 3))
                                           ""
                                         (format "r%d" (cl-incf n)))))
                           (push 'row trace))
                        (_ nil))
                      (check step nil)
                      (setq items (inc-rnd--items items chat))
                      (push items trace)
                      (check step t))))
              (when (get-buffer "*inc-rnd*")
                (kill-buffer "*inc-rnd*"))))))
      (expect failure :to-be nil))))

(describe "incremental component-list patching around a live stream"
  ;; A `vui-stream' grows by edits that are not renders of the component
  ;; that placed it, so the text of that component is no longer what its
  ;; last render wrote.  The patcher must still find where it ends.

  (it "shows the first item of a stream that was empty when the list was recorded"
    ;; The empty stream writes nothing, so its component has no
    ;; separator after it: the box starts the buffer
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-first-chat (stream)
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-first-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-first (stream tick)
        :render (vui-vstack
                 (vui-component 'inc-cs-first-chat :stream stream)
                 (vui-component 'inc-cs-first-box :tick tick)))
      (let ((inst (vui-mount (vui-component 'inc-cs-first :stream s :tick 0)
                             "*inc-cs-first*")))
        (unwind-protect
            (with-current-buffer "*inc-cs-first*"
              (vui-rerender inst)
              (expect (plist-get (vui-instance-render-record inst) :kind)
                      :to-be 'components)
              (vui-stream-append s (vui-text "a"))
              (expect (buffer-string) :to-equal "a\nbox 0")
              (vui-stream-append s (vui-text "b"))
              (expect (buffer-string) :to-equal "a\nb\nbox 0"))
          (kill-buffer "*inc-cs-first*")))))

  (it "finds the text of a child whose stream grew"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-chat (stream)
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-shell (stream tick)
        :render (vui-vstack
                 (vui-component 'inc-cs-chat :key "chat" :stream stream)
                 (vui-component 'inc-cs-box :key "box" :tick tick)))
      (let ((inst (vui-mount (vui-component 'inc-cs-shell :stream s :tick 0)
                             "*inc-cs*")))
        (unwind-protect
            (with-current-buffer "*inc-cs*"
              ;; The first item re-renders the root, which records the list
              (vui-stream-append s (vui-text "a"))
              (vui-update inst (list :stream s :tick 1))
              (expect (plist-get (vui-instance-render-record inst) :kind)
                      :to-be 'components)
              (expect (buffer-string) :to-equal "a\nbox 1")
              (vui-stream-append s (vui-text "b"))
              (vui-update inst (list :stream s :tick 2))
              (expect (buffer-string) :to-equal "a\nb\nbox 2")
              ;; The last item grows
              (vui-stream-update-last s (vui-text "b and more"))
              (vui-update inst (list :stream s :tick 3))
              (expect (buffer-string) :to-equal "a\nb and more\nbox 3")
              ;; The last item is replaced by a shorter one
              (vui-stream-update-last s (vui-text "c"))
              (vui-update inst (list :stream s :tick 4))
              (expect (buffer-string) :to-equal "a\nc\nbox 4"))
          (kill-buffer "*inc-cs*")))))

  (it "finds the text of a child whose nested component placed the stream"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-log (stream)
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-panel (stream)
        :render (vui-vstack (vui-text "log:")
                            (vui-component 'inc-cs-log :stream stream)
                            (vui-text "end of log")))
      (vui-defcomponent inc-cs-panel-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-panel-shell (stream tick)
        :render (vui-vstack
                 (vui-component 'inc-cs-panel :key "panel" :stream stream)
                 (vui-component 'inc-cs-panel-box :key "box" :tick tick)))
      (let ((inst (vui-mount (vui-component 'inc-cs-panel-shell
                               :stream s :tick 0)
                             "*inc-cs-panel*")))
        (unwind-protect
            (with-current-buffer "*inc-cs-panel*"
              (vui-stream-append s (vui-text "a"))
              (vui-update inst (list :stream s :tick 1))
              (vui-stream-append s (vui-text "b"))
              (vui-update inst (list :stream s :tick 2))
              (expect (buffer-string)
                      :to-equal "log:\na\nb\nend of log\nbox 2"))
          (kill-buffer "*inc-cs-panel*")))))

  (it "skips the text of a child that bails out while its stream grows"
    ;; The child never renders again, so its text is the one in the
    ;; buffer, and stream nodes keep their markers.
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-kept-chat (stream)
        :should-update nil
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-kept-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-kept-shell (stream tick)
        :render (vui-vstack
                 (vui-component 'inc-cs-kept-chat :key "chat" :stream stream)
                 (vui-component 'inc-cs-kept-box :key "box" :tick tick)))
      (let ((inst (vui-mount (vui-component 'inc-cs-kept-shell
                               :stream s :tick 0)
                             "*inc-cs-kept*")))
        (unwind-protect
            (with-current-buffer "*inc-cs-kept*"
              (vui-stream-append s (vui-text "a"))
              (vui-update inst (list :stream s :tick 1))
              (expect (buffer-string) :to-equal "a\nbox 1")
              (vui-stream-append s (vui-text "b"))
              (vui-update inst (list :stream s :tick 2))
              (expect (buffer-string) :to-equal "a\nb\nbox 2")
              (vui-stream-update-last s (vui-text "b and more"))
              (vui-update inst (list :stream s :tick 3))
              (expect (buffer-string) :to-equal "a\nb and more\nbox 3")
              (let ((node (vui-stream-open s (vui-text "c"))))
                (vui-stream-append-to node (vui-text "d"))
                (vui-update inst (list :stream s :tick 4))
                (expect (buffer-string) :to-equal "a\nb and more\ncd\nbox 4")
                ;; The node is still where its text is
                (vui-stream-update node (vui-text "e"))
                (vui-update inst (list :stream s :tick 5))
                (expect (buffer-string) :to-equal "a\nb and more\ne\nbox 5")))
          (kill-buffer "*inc-cs-kept*")))))

  (it "skips the text of a child whose stream rows re-render on their own"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-row (label)
        :state ((open nil))
        :render (vui-text (if open (format "%s\n  details" label) label)))
      (vui-defcomponent inc-cs-row-chat (stream)
        :should-update nil
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-row-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-row-shell (stream tick)
        :render (vui-vstack
                 (vui-component 'inc-cs-row-chat :key "chat" :stream stream)
                 (vui-component 'inc-cs-row-box :key "box" :tick tick)))
      (let ((inst (vui-mount (vui-component 'inc-cs-row-shell
                               :stream s :tick 0)
                             "*inc-cs-row*")))
        (unwind-protect
            (with-current-buffer "*inc-cs-row*"
              (vui-stream-append s (vui-text "a"))
              (vui-update inst (list :stream s :tick 1))
              (vui-stream-append s (vui-component 'inc-cs-row :label "tool"))
              (vui-update inst (list :stream s :tick 2))
              (expect (buffer-string) :to-equal "a\ntool\nbox 2")
              (let ((row (car (vui-stream-handle-items-rev s))))
                (expect (vui-instance-p row) :to-be-truthy)
                ;; The row opens, as from a handler of its own: only its
                ;; region renders
                (let ((vui--current-instance row)
                      (vui--root-instance row))
                  (vui-set-state :open t))
                (expect (buffer-string) :to-equal "a\ntool\n  details\nbox 2")
                (vui-update inst (list :stream s :tick 3))
                (expect (buffer-string) :to-equal "a\ntool\n  details\nbox 3")
                ;; The row gets new props
                (vui-stream-update-last
                 s (vui-component 'inc-cs-row :label "tool done"))
                (vui-update inst (list :stream s :tick 4))
                (expect (buffer-string)
                        :to-equal "a\ntool done\n  details\nbox 4")))
          (kill-buffer "*inc-cs-row*")))))

  (it "removes the text of a child whose stream grew"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent inc-cs-gone-chat (stream)
        :render (vui-stream stream))
      (vui-defcomponent inc-cs-gone-box (tick)
        :render (vui-text (format "box %s" tick)))
      (vui-defcomponent inc-cs-gone-shell (stream tick chat)
        :render (vui-vstack
                 (vui-component 'inc-cs-gone-box :key "box" :tick tick)
                 (when chat
                   (vui-component 'inc-cs-gone-chat :key "chat"
                     :stream stream))))
      (let ((inst (vui-mount (vui-component 'inc-cs-gone-shell
                               :stream s :tick 0 :chat t)
                             "*inc-cs-gone*")))
        (unwind-protect
            (with-current-buffer "*inc-cs-gone*"
              (vui-stream-append s (vui-text "a"))
              (vui-update inst (list :stream s :tick 1 :chat t))
              (vui-stream-append s (vui-text "b"))
              (expect (buffer-string) :to-equal "box 1\na\nb")
              (vui-update inst (list :stream s :tick 2 :chat nil))
              (expect (buffer-string) :to-equal "box 2"))
          (kill-buffer "*inc-cs-gone*"))))))

(describe "incremental rendering falls back for ineligible trees"
  (it "renders an indented vstack correctly (wholesale fallback)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-indent (n)
        :render (vui-vstack :indent 2
                  (vui-text (format "x%d" n))))
      (let ((inst (vui-mount (vui-component 'inc-indent :n 0) "*inc-ind*")))
        (unwind-protect
            (with-current-buffer "*inc-ind*"
              (vui-update inst '(:n 1))
              (expect (buffer-string) :to-equal "  x1"))
          (kill-buffer "*inc-ind*")))))

  (it "handles a tree that toggles between eligible and ineligible"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-toggle (mode)
        :render (if (eq mode 'text)
                    (vui-vstack (vui-text "one") (vui-text "two"))
                  (vui-vstack (vui-button "btn" :on-click #'ignore))))
      (let ((inst (vui-mount (vui-component 'inc-toggle :mode 'text) "*inc-tog*")))
        (unwind-protect
            (with-current-buffer "*inc-tog*"
              (vui-update inst (list :mode 'text))
              (expect (buffer-string) :to-equal "one\ntwo")
              (vui-update inst (list :mode 'widget))   ; -> ineligible (button)
              (expect (buffer-string) :to-match "btn")
              (vui-update inst (list :mode 'text))     ; -> eligible again
              (expect (buffer-string) :to-equal "one\ntwo"))
          (kill-buffer "*inc-tog*"))))))

(describe "incremental :memo shorthand"
  (it "bails out unchanged memo children (only the changed one re-renders)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-memo-child (id label)
        :memo t
        :render (progn (setq renders (1+ renders))
                       (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-memo-list (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-memo-child
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-memo-list
                                            :items '((1 . "a") (2 . "b") (3 . "c")))
                             "*inc-memo*")))
        (unwind-protect
            (with-current-buffer "*inc-memo*"
              (expect (buffer-string) :to-equal "[1:a]\n[2:b]\n[3:c]")
              (setq renders 0)
              ;; only item 2 changes; memo must skip 1 and 3
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 1)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]")
              ;; no change at all: memo skips every child
              (setq renders 0)
              (vui-update inst '(:items ((1 . "a") (2 . "B") (3 . "c"))))
              (expect renders :to-equal 0)
              (expect (buffer-string) :to-equal "[1:a]\n[2:B]\n[3:c]"))
          (kill-buffer "*inc-memo*")))))

  (it "re-renders a memo child when its own state changes (not just props)"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (vui-defcomponent inc-memo-stateful (id)
        :memo t
        :state ((n 0))
        :render (vui-text (format "[%s:%d]" id n)))
      (vui-defcomponent inc-memo-slist (ids)
        :render (apply #'vui-vstack
                       (mapcar (lambda (id)
                                 (vui-component 'inc-memo-stateful :key id :id id))
                               ids)))
      (let ((inst (vui-mount (vui-component 'inc-memo-slist :ids '("a" "b"))
                             "*inc-memo-s*")))
        (unwind-protect
            (with-current-buffer "*inc-memo-s*"
              (expect (buffer-string) :to-equal "[a:0]\n[b:0]")
              (let ((a (cl-find "a" (vui-get-component-instances 'inc-memo-stateful inst)
                                :key (lambda (i) (plist-get (vui-instance-props i) :id))
                                :test #'equal)))
                (let ((vui--current-instance a)) (vui-set-state :n 7)))
              ;; props unchanged but state changed -> must NOT bail
              (expect (buffer-string) :to-equal "[a:7]\n[b:0]"))
          (kill-buffer "*inc-memo-s*")))))

  (it "lets an explicit :should-update override :memo"
    (let ((vui-incremental-render t)
          (vui-render-delay nil)
          (renders 0))
      ;; :should-update t forces a render every time, despite :memo t
      (vui-defcomponent inc-memo-override (id label)
        :memo t
        :should-update t
        :render (progn (setq renders (1+ renders))
                       (vui-text (format "[%s:%s]" id label))))
      (vui-defcomponent inc-memo-olist (items)
        :render (apply #'vui-vstack
                       (mapcar (lambda (it)
                                 (vui-component 'inc-memo-override
                                   :key (car it) :id (car it) :label (cdr it)))
                               items)))
      (let ((inst (vui-mount (vui-component 'inc-memo-olist
                                            :items '((1 . "a") (2 . "b")))
                             "*inc-memo-o*")))
        (unwind-protect
            (with-current-buffer "*inc-memo-o*"
              (setq renders 0)
              ;; nothing changed, but should-update=t forces both to render
              (vui-update inst '(:items ((1 . "a") (2 . "b"))))
              (expect renders :to-equal 2))
          (kill-buffer "*inc-memo-o*"))))))

(describe ":memo in wholesale mode (flag off)"
  (it "skips a child's vnode production when its props are unchanged"
    (let ((vui-incremental-render nil)
          (vui-render-delay nil)
          (renders 0))
      (vui-defcomponent inc-memo-ws (label)
        :memo t
        :render (progn (setq renders (1+ renders)) (vui-text label)))
      (vui-defcomponent inc-memo-ws-parent (label)
        :state ((tick 0))
        :render (vui-vstack
                 (vui-text (format "tick%d" tick))
                 (vui-component 'inc-memo-ws :label label)))
      (let ((inst (vui-mount (vui-component 'inc-memo-ws-parent :label "x")
                             "*memo-ws*")))
        (unwind-protect
            (with-current-buffer "*memo-ws*"
              (setq renders 0)
              ;; bump the parent's state; the child's prop is unchanged, so
              ;; its render-fn must not run even on the wholesale path
              (let ((vui--current-instance inst)) (vui-set-state :tick 1))
              (expect renders :to-equal 0)
              (expect (buffer-string) :to-equal "tick1\nx"))
          (kill-buffer "*memo-ws*")))))

  (it "re-renders a memo child when a string prop changes only in text properties"
    (let ((vui-incremental-render nil)
          (vui-render-delay nil))
      (vui-defcomponent inc-memo-ws-face (label)
        :memo t
        :render (vui-text label))
      (vui-defcomponent inc-memo-ws-face-parent (label)
        :render (vui-component 'inc-memo-ws-face :label label))
      (let ((inst (vui-mount (vui-component 'inc-memo-ws-face-parent
                               :label (propertize "hot" 'face 'error))
                             "*memo-ws-face*")))
        (unwind-protect
            (with-current-buffer "*memo-ws-face*"
              (expect (get-text-property (point-min) 'face) :to-equal 'error)
              ;; same characters, different face: :memo must NOT bail out
              (vui-update inst (list :label (propertize "hot" 'face 'success)))
              (expect (get-text-property (point-min) 'face) :to-equal 'success))
          (kill-buffer "*memo-ws-face*")))))

  (it "re-renders a memo child when a state string changes only in text properties"
    (let ((vui-incremental-render nil)
          (vui-render-delay nil))
      (vui-defcomponent inc-memo-ws-sface ()
        :memo t
        :state ((s (propertize "hot" 'face 'error)))
        :render (vui-text s))
      (vui-defcomponent inc-memo-ws-sface-parent ()
        :render (vui-component 'inc-memo-ws-sface))
      (let ((inst (vui-mount (vui-component 'inc-memo-ws-sface-parent)
                             "*memo-ws-sface*")))
        (unwind-protect
            (with-current-buffer "*memo-ws-sface*"
              (expect (get-text-property (point-min) 'face) :to-equal 'error)
              (let ((child (car (vui-get-component-instances
                                 'inc-memo-ws-sface inst))))
                (let ((vui--current-instance child))
                  (vui-set-state :s (propertize "hot" 'face 'success))))
              ;; same characters, different face: :memo must NOT bail out
              (expect (get-text-property (point-min) 'face) :to-equal 'success))
          (kill-buffer "*memo-ws-sface*"))))))

(provide 'vui-incremental-test)
;;; vui-incremental-test.el ends here

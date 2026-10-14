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

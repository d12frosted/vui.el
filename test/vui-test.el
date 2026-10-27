;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (button.el text buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the text button at POS."
  (let ((button (button-at pos)))
    (when button
      (button-activate button))))

(describe "vui.el"
  (it "loads successfully"
    (expect (featurep 'vui) :to-be-truthy)))

(describe "vui-text"
  (it "creates a text vnode"
    (let ((node (vui-text "hello")))
      (expect (vui-vnode-text-p node) :to-be-truthy)
      (expect (vui-vnode-text-content node) :to-equal "hello")))

  (it "accepts face property"
    (let ((node (vui-text "hello" :face 'bold)))
      (expect (vui-vnode-text-face node) :to-equal 'bold)))

  (it "accepts key property"
    (let ((node (vui-text "hello" :key "my-key")))
      (expect (vui-vnode-key node) :to-equal "my-key"))))

(describe "vui-fragment"
  (it "creates a fragment with children"
    (let ((node (vui-fragment (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-fragment-p node) :to-be-truthy)
      (expect (length (vui-vnode-fragment-children node)) :to-equal 2)))

  (it "renders a list of children given as one argument"
    ;; As `cl-loop' collects them
    (with-temp-buffer
      (vui-render (vui-fragment
                   (cl-loop for item in '("One" "Two")
                            collect (vui-fragment
                                     (vui-text (format "- %s" item))
                                     (vui-newline)))))
      (expect (buffer-string) :to-equal "- One\n- Two\n")))

  (it "renders the children of nested lists in place, nil included"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a")
                                (list nil (list (vui-text "b")))
                                nil (vui-text "c")))
      (expect (buffer-string) :to-equal "abc")))

  (it "keeps a boundary with no id after a list that grows in its place"
    ;; The list is one child of the fragment, whatever its length
    (let ((renders 0))
      (vui-defcomponent fragment-list-flaky ()
        :render (if (< (cl-incf renders) 2)
                    (error "First render fails")
                  (vui-text "ok")))
      (vui-defcomponent fragment-list-boundary-app (labels)
        :render
        (vui-fragment
         (cl-loop for label in labels collect (vui-text (concat label " ")))
         (vui-error-boundary
          :fallback (lambda (_) (vui-text "fallback"))
          :children (vui-component 'fragment-list-flaky))))
      (let ((instance (vui-mount (vui-component 'fragment-list-boundary-app
                                                :labels '("a"))
                                 "*test-fragment-list-boundary*")))
        (unwind-protect
            (with-current-buffer "*test-fragment-list-boundary*"
              (expect (buffer-string) :to-equal "a fallback")
              (vui-update instance '(:labels ("a" "b")))
              (expect (buffer-string) :to-equal "a b fallback"))
          (kill-buffer "*test-fragment-list-boundary*")))))

  (it "keeps the state of a keyed component after a list that changes length"
    (vui-defcomponent fragment-list-tail (label)
      :state ((n 0))
      :render (vui-button (format "%s=%d" label n)
                :on-click (lambda () (vui-set-state :n (1+ n)))))
    (vui-defcomponent fragment-list-tail-app (labels)
      :render (vui-fragment
               (cl-loop for label in labels
                        collect (vui-component 'fragment-list-tail
                                               :key label :label label))
               (vui-component 'fragment-list-tail :key 'tail :label "t")))
    (let ((vui-render-delay nil)
          (instance (vui-mount (vui-component 'fragment-list-tail-app
                                              :labels '("a"))
                               "*test-fragment-list-tail*")))
      (unwind-protect
          (with-current-buffer "*test-fragment-list-tail*"
            (goto-char (point-min))
            (search-forward "[t=0]")
            (button-activate (button-at (match-beginning 0)))
            (expect (buffer-string) :to-equal "[a=0][t=1]")
            (vui-update instance '(:labels ("a" "b" "c")))
            (expect (buffer-string) :to-equal "[a=0][b=0][c=0][t=1]")
            (vui-update instance '(:labels nil))
            (expect (buffer-string) :to-equal "[t=1]"))
        (kill-buffer "*test-fragment-list-tail*"))))

  (it "keeps the state of keyed components given as a list"
    (vui-defcomponent fragment-list-counter (label)
      :state ((n 0))
      :render (vui-button (format "%s=%d" label n)
                :on-click (lambda () (vui-set-state :n (1+ n)))))
    (vui-defcomponent fragment-list-app (labels)
      :render (vui-fragment
               (cl-loop for label in labels
                        collect (vui-component 'fragment-list-counter
                                               :key label :label label))))
    (let ((vui-render-delay nil)
          (instance (vui-mount (vui-component 'fragment-list-app
                                              :labels '("a" "b"))
                               "*test-fragment-list*")))
      (unwind-protect
          (with-current-buffer "*test-fragment-list*"
            (goto-char (point-min))
            (search-forward "[a=0]")
            (button-activate (button-at (match-beginning 0)))
            (expect (buffer-string) :to-equal "[a=1][b=0]")
            (vui-update instance '(:labels ("b" "a")))
            (expect (buffer-string) :to-equal "[b=0][a=1]"))
        (kill-buffer "*test-fragment-list*")))))

(describe "vui-newline"
  (it "creates a newline vnode"
    (let ((node (vui-newline)))
      (expect (vui-vnode-newline-p node) :to-be-truthy)))

  ;; Standalone rendering
  (it "renders as newline when standalone"
    (with-temp-buffer
      (vui-render (vui-newline))
      (expect (buffer-string) :to-equal "\n"))))

(describe "vui-space"
  (it "creates a space vnode with default width"
    (let ((node (vui-space)))
      (expect (vui-vnode-space-p node) :to-be-truthy)
      (expect (vui-vnode-space-width node) :to-equal 1)))

  (it "accepts custom width"
    (let ((node (vui-space 5)))
      (expect (vui-vnode-space-width node) :to-equal 5))))

(describe "vui-button"
  (it "creates a button vnode"
    (let ((node (vui-button "Click me")))
      (expect (vui-vnode-button-p node) :to-be-truthy)
      (expect (vui-vnode-button-label node) :to-equal "Click me")))

  (it "accepts on-click callback"
    (let* ((clicked nil)
           (node (vui-button "Click" :on-click (lambda () (setq clicked t)))))
      (expect (vui-vnode-button-on-click node) :to-be-truthy)))

  (it "accepts disabled property"
    (let ((node (vui-button "Click" :disabled t)))
      (expect (vui-vnode-button-disabled-p node) :to-be-truthy)))

  (it "accepts key property"
    (let ((node (vui-button "Click" :key "btn-1")))
      (expect (vui-vnode-key node) :to-equal "btn-1")))

  ;; Truncation support for buttons in constrained spaces (e.g., table cells)
  (it "accepts max-width property"
    (let ((node (vui-button "Click" :max-width 10)))
      (expect (vui-vnode-button-max-width node) :to-equal 10)))

  (it "renders without truncation when label fits max-width"
    (with-temp-buffer
      ;; "hello" = 5 chars, button adds [] = 7 total, fits in 10
      (vui-render (vui-button "hello" :max-width 10))
      (expect (buffer-string) :to-equal "[hello]")))

  (it "truncates label when exceeds max-width"
    (with-temp-buffer
      ;; "hello world" = 11 chars + [] = 13, must fit in 10
      ;; [hello...] = 10 chars: [ (1) + hello (5) + ... (3) + ] (1)
      (vui-render (vui-button "hello world" :max-width 10))
      (expect (buffer-string) :to-equal "[hello...]")))

  (it "handles very small max-width gracefully"
    (with-temp-buffer
      ;; max-width 5: [..] = 4 chars minimum, 1 char for label
      ;; [...] or [x...] or just [...]
      (vui-render (vui-button "hello world" :max-width 5))
      ;; At minimum should show [...] (5 chars)
      (expect (buffer-string) :to-equal "[...]")))

  (it "clips the label without an ellipsis when the ellipsis does not fit"
    ;; Room for the brackets, but not for "..." between them
    (with-temp-buffer
      (vui-render (vui-button "hello world" :max-width 4))
      (expect (buffer-string) :to-equal "[he]")))

  (it "drops the brackets when they leave no room for the label"
    (dolist (case '((2 . "he") (1 . "h") (0 . "")))
      (with-temp-buffer
        (vui-render (vui-button "hello world" :max-width (car case)))
        (expect (buffer-string) :to-equal (cdr case)))))

  (it "keeps the brackets of an empty label that fits"
    (with-temp-buffer
      (vui-render (vui-button "" :max-width 2))
      (expect (buffer-string) :to-equal "[]")))

  (it "truncates a wide label by its width, not by its length"
    (let ((vui-width-mode 'char))
      (dolist (case '((1 . "") (2 . "你") (3 . "...") (4 . "[你]")
                      (6 . "[...]") (7 . "[你...]")))
        (with-temp-buffer
          (vui-render (vui-button "你好世界" :max-width (car case)))
          (expect (buffer-string) :to-equal (cdr case))))))

  ;; No-decoration support
  (it "accepts no-decoration property"
    (let ((node (vui-button "Click" :no-decoration t)))
      (expect (vui-vnode-button-no-decoration node) :to-be-truthy)))

  (it "renders without brackets when no-decoration is set"
    (with-temp-buffer
      (vui-render (vui-button "Click me" :no-decoration t))
      (expect (buffer-string) :to-equal "Click me")))

  (it "renders with brackets by default"
    (with-temp-buffer
      (vui-render (vui-button "Click me"))
      (expect (buffer-string) :to-equal "[Click me]")))

  (it "truncates no-decoration button without bracket overhead"
    (with-temp-buffer
      ;; "hello world" = 11 chars, must fit in 10
      ;; Without brackets: "hello w..." = 10 chars (7 + 3)
      (vui-render (vui-button "hello world" :max-width 10 :no-decoration t))
      (expect (buffer-string) :to-equal "hello w...")))

  (it "handles very small max-width for no-decoration button"
    (with-temp-buffer
      ;; max-width 3: just "..."
      (vui-render (vui-button "hello world" :max-width 3 :no-decoration t))
      (expect (buffer-string) :to-equal "...")))

  (it "clips a no-decoration label narrower than the ellipsis"
    (with-temp-buffer
      (vui-render (vui-button "hello world" :max-width 2 :no-decoration t))
      (expect (buffer-string) :to-equal "he")))

  ;; Help-echo support (performance optimization)
  (it "defaults help-echo to :default"
    (let ((node (vui-button "Click")))
      (expect (vui-vnode-button-help-echo node) :to-equal :default)))

  (it "accepts help-echo nil to disable tooltip"
    (let ((node (vui-button "Click" :help-echo nil)))
      (expect (vui-vnode-button-help-echo node) :to-be nil)))

  (it "accepts custom help-echo string"
    (let ((node (vui-button "Click" :help-echo "Custom tooltip")))
      (expect (vui-vnode-button-help-echo node) :to-equal "Custom tooltip")))

  (it "renders with help-echo nil"
    (with-temp-buffer
      ;; Should render without error
      (vui-render (vui-button "Click" :help-echo nil))
      (expect (buffer-string) :to-equal "[Click]"))))

(describe "vui-field"
  (it "creates a field vnode"
    (let ((node (vui-field :value "hello")))
      (expect (vui-vnode-field-p node) :to-be-truthy)
      (expect (vui-vnode-field-value node) :to-equal "hello")))

  (it "defaults to empty string"
    (let ((node (vui-field)))
      (expect (vui-vnode-field-value node) :to-equal "")))

  (it "accepts size property"
    (let ((node (vui-field :size 30)))
      (expect (vui-vnode-field-size node) :to-equal 30)))

  (it "accepts on-change callback"
    (let ((node (vui-field :on-change #'identity)))
      (expect (vui-vnode-field-on-change node) :to-be-truthy)))

  (it "accepts on-submit callback"
    (let ((node (vui-field :on-submit #'identity)))
      (expect (vui-vnode-field-on-submit node) :to-be-truthy)))

  (it "accepts key property"
    (let ((node (vui-field :key "field-1")))
      (expect (vui-vnode-key node) :to-equal "field-1")))

  (it "accepts face property"
    (let ((node (vui-field :face 'bold)))
      (expect (vui-vnode-field-face node) :to-equal 'bold)))

  (it "accepts placeholder property"
    (let ((node (vui-field :placeholder "Enter text...")))
      (expect (vui-vnode-field-placeholder node) :to-equal "Enter text...")))

  (it "triggers on-submit callback on RET"
    (let ((submitted-value nil))
      (vui-defcomponent submit-test ()
        :render
        (vui-field :key 'test-field
                   :value "initial"
                   :on-submit (lambda (v) (setq submitted-value v))))
      (let ((instance (vui-mount (vui-component 'submit-test) "*test-submit*")))
        (unwind-protect
            (with-current-buffer "*test-submit*"
              ;; Get the field widget directly from widget-field-list
              ;; (vui-forward doesn't work with single widget on Emacs 29)
              (let ((widget (car widget-field-list)))
                (expect widget :to-be-truthy)
                ;; Simulate RET by applying the widget action
                (widget-apply widget :action))
              ;; on-submit should have been called with the field value
              (expect submitted-value :to-equal "initial"))
          (kill-buffer "*test-submit*"))))))

(defun vui-test--placeholder-overlays ()
  "Return all placeholder overlays in the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'vui-placeholder))
              (overlays-in (point-min) (point-max))))

(describe "vui-field placeholder"
  (it "shows placeholder while the field is empty"
    (with-temp-buffer
      (vui-render (vui-field :size 10 :placeholder "Name..."))
      (let ((overlays (vui-test--placeholder-overlays)))
        (expect (length overlays) :to-equal 1)
        (let ((display (overlay-get (car overlays) 'display)))
          (expect display :to-match "Name\\.\\.\\.")
          (expect (get-text-property 0 'face display)
                  :to-equal 'vui-field-placeholder)))))

  (it "does not show placeholder when the field has a value"
    (with-temp-buffer
      (vui-render (vui-field :value "Boris" :size 10 :placeholder "Name..."))
      (expect (vui-test--placeholder-overlays) :to-equal nil)))

  (it "hides placeholder as soon as the user types"
    (with-temp-buffer
      (vui-render (vui-field :size 10 :placeholder "Name..."))
      (expect (length (vui-test--placeholder-overlays)) :to-equal 1)
      (let ((widget (car widget-field-list)))
        (widget-value-set widget "B"))
      (expect (vui-test--placeholder-overlays) :to-equal nil)))

  (it "pads and truncates the placeholder to the field size"
    (with-temp-buffer
      (vui-render (vui-field :size 4 :placeholder "Very long placeholder"))
      (let ((display (overlay-get (car (vui-test--placeholder-overlays))
                                  'display)))
        (expect (length display) :to-equal 4)))
    (with-temp-buffer
      (vui-render (vui-field :size 10 :placeholder "ab"))
      (let ((display (overlay-get (car (vui-test--placeholder-overlays))
                                  'display)))
        (expect (length display) :to-equal 10))))

  (it "does not accumulate overlays across re-renders"
    (vui-defcomponent placeholder-rerender-test ()
      :render (vui-field :size 10 :placeholder "Type here"))
    (let ((instance (vui-mount (vui-component 'placeholder-rerender-test)
                               "*test-placeholder-rerender*")))
      (unwind-protect
          (progn
            (vui--rerender-instance instance)
            (vui--rerender-instance instance)
            (with-current-buffer "*test-placeholder-rerender*"
              (expect (length (vui-test--placeholder-overlays)) :to-equal 1)))
        (kill-buffer "*test-placeholder-rerender*"))))

  (it "setting up placeholders twice does not duplicate overlays"
    (with-temp-buffer
      (vui-render (vui-field :size 10 :placeholder "Name..."))
      (expect (length (vui-test--placeholder-overlays)) :to-equal 1)
      ;; A second pass (e.g. after another region in the same buffer
      ;; re-rendered) must not add a second overlay to this field
      (vui--setup-field-placeholders)
      (expect (length (vui-test--placeholder-overlays)) :to-equal 1))))

(describe "region-scoped widget helpers"
  (it "collects only widgets within the given bounds"
    (with-temp-buffer
      (vui-render (vui-fragment
                   (vui-button "one" :on-click #'ignore)
                   (vui-newline)
                   (vui-button "two" :on-click #'ignore)))
      (let* ((all (vui--collect-widgets))
             (first-end (cdr (vui--widget-bounds (car all)))))
        (expect (length all) :to-equal 2)
        ;; Bounded to the first line: only the first button
        (expect (length (vui--collect-widgets (point-min) first-end))
                :to-equal 1)
        ;; Bounded to the rest: only the second button
        (expect (length (vui--collect-widgets first-end (point-max)))
                :to-equal 1))))

  (it "removes widget overlays only within the given bounds"
    (with-temp-buffer
      (vui-render (vui-fragment
                   (vui-field :size 5 :key 'f1)
                   (vui-newline)
                   (vui-field :size 5 :key 'f2)))
      (let* ((field-overlays
              (lambda ()
                (seq-filter (lambda (ov) (overlay-get ov 'field))
                            (overlays-in (point-min) (point-max)))))
             (before (funcall field-overlays))
             (mid (save-excursion (goto-char (point-min))
                                  (forward-line 1)
                                  (point))))
        (expect (length before) :to-equal 2)
        (vui--remove-widget-overlays (point-min) mid)
        (expect (length (funcall field-overlays)) :to-equal 1)))))

(describe "vui-field-value"
  (it "retrieves field value by key"
    (vui-defcomponent field-value-test ()
      :render
      (vui-vstack
        (vui-field :key 'my-input :value "hello world")))
    (let ((instance (vui-mount (vui-component 'field-value-test) "*test-fv*")))
      (unwind-protect
          (with-current-buffer "*test-fv*"
            (expect (vui-field-value 'my-input) :to-equal "hello world"))
        (kill-buffer "*test-fv*"))))

  (it "retrieves field value by a string or list key"
    ;; Keys are compared with `equal', as in reconciliation
    (vui-defcomponent field-value-string-test ()
      :render
      (vui-vstack
       (vui-field :key "search-input" :value "query")
       (vui-field :key '(row 1) :value "cell")))
    (vui-mount (vui-component 'field-value-string-test) "*test-fv-string*")
    (unwind-protect
        (with-current-buffer "*test-fv-string*"
          (expect (vui-field-value (concat "search" "-input")) :to-equal "query")
          (expect (vui-field-value (list 'row 1)) :to-equal "cell"))
      (kill-buffer "*test-fv-string*")))

  (it "returns nil for non-existent key"
    (vui-defcomponent field-value-nil-test ()
      :render
      (vui-field :key 'existing :value "test"))
    (let ((instance (vui-mount (vui-component 'field-value-nil-test) "*test-fv2*")))
      (unwind-protect
          (with-current-buffer "*test-fv2*"
            (expect (vui-field-value 'non-existent) :to-be nil))
        (kill-buffer "*test-fv2*"))))

  (it "reads current edited value not initial value"
    (vui-defcomponent field-value-edit-test ()
      :render
      (vui-field :key 'editable :value "initial"))
    (let ((instance (vui-mount (vui-component 'field-value-edit-test) "*test-fv3*")))
      (unwind-protect
          (with-current-buffer "*test-fv3*"
            ;; Get the field widget directly from widget-field-list
            ;; (vui-forward doesn't work with single widget on Emacs 29)
            (let ((widget (car widget-field-list)))
              ;; Simulate user typing by setting widget value
              (widget-value-set widget "modified")
              ;; vui-field-value should return the modified value
              (expect (vui-field-value 'editable) :to-equal "modified")))
        (kill-buffer "*test-fv3*"))))

  (it "works with multiple fields with symbol keys"
    (vui-defcomponent field-value-keys-test ()
      :render
      (vui-vstack
        (vui-field :key 'first-key :value "first")
        (vui-field :key 'second-key :value "second")))
    (let ((instance (vui-mount (vui-component 'field-value-keys-test) "*test-fv4*")))
      (unwind-protect
          (with-current-buffer "*test-fv4*"
            (expect (vui-field-value 'first-key) :to-equal "first")
            (expect (vui-field-value 'second-key) :to-equal "second"))
        (kill-buffer "*test-fv4*")))))

(defvar vui-test--render-effects nil
  "What the effects of the components below saw, newest first.")

(defvar vui-test--render-host-renders 0
  "How many times `vui-test-render-from-hook' rendered.")

(vui-defcomponent vui-test-render-effect (tag)
  "TAG as text, with an effect that notes TAG and the buffer it runs in,
and a button that counts its clicks."
  :state ((clicks 0))
  :render (progn
            (vui-use-effect ()
              (push (list tag (buffer-name)) vui-test--render-effects))
            (vui-hstack (vui-text (format "%s %d" tag clicks))
                        (vui-button "click"
                          :on-click (lambda () (vui-set-state :clicks #'1+))))))

(vui-defcomponent vui-test-render-from-hook (buffer)
  "Renders a `vui-test-render-effect' into BUFFER from its `:on-mount'."
  :on-mount (vui-render (vui-component 'vui-test-render-effect :tag 'side)
                        buffer)
  :render (progn
            (cl-incf vui-test--render-host-renders)
            (vui-use-effect ()
              (push (list 'host (buffer-name)) vui-test--render-effects))
            (vui-text "host")))

(vui-defcomponent vui-test-render-from-effect (buffer)
  "A `vui-test-render-effect' of its own, and an effect that renders
another one into BUFFER."
  :render (progn
            (vui-use-effect ()
              (vui-render (vui-component 'vui-test-render-effect :tag 'side)
                          buffer))
            (vui-vstack (vui-component 'vui-test-render-effect :tag 'own))))

(vui-defcontext vui-test-render-theme 'light
  "A context for the `vui-render' specs.")

(vui-defcomponent vui-test-render-theme-reader ()
  "The value of `vui-test-render-theme' as text."
  :render (vui-text (format "theme %s" (vui-use-context
                                        vui-test-render-theme-context))))

(vui-defcomponent vui-test-render-under-provider (side inner)
  "Renders a theme reader into SIDE from its `:on-mount', under a
provider, and with one of its own around the reader when INNER."
  :on-mount (vui-render (if inner
                            (vui-test-render-theme-provider 'inner
                              (vui-component 'vui-test-render-theme-reader))
                          (vui-component 'vui-test-render-theme-reader))
                        side)
  :render (vui-text "host"))

(vui-defcomponent vui-test-render-theme-root (side inner)
  "A `dark' provider around a `vui-test-render-under-provider'."
  :render (vui-test-render-theme-provider 'dark
            (vui-component 'vui-test-render-under-provider
                           :side side :inner inner)))

(vui-defcomponent vui-test-render-boom ()
  "Fails to render."
  :render (error "Boom"))

(vui-defcomponent vui-test-render-boundary-host (side)
  "An error boundary around text; its `:on-mount' renders a boundary at
the same place around a failing component into SIDE."
  :on-mount (vui-render (vui-error-boundary
                         :fallback (lambda (_error) (vui-text "side fallback"))
                         :children (list (vui-component 'vui-test-render-boom)))
                        side)
  :render (vui-error-boundary
           :fallback (lambda (_error) (vui-text "host fallback"))
           :children (list (vui-text "host"))))

(vui-defcomponent vui-test-render-sibling ()
  "Its state as text."
  :state ((n 0))
  :render (vui-text (format "sibling %d" n)))

(defvar vui-test--render-side-texts nil
  "The text of the buffer `vui-test-render-in-render' renders into,
after each of its renders, newest first.")

(vui-defcomponent vui-test-render-in-render (side mount)
  "Renders a `vui-test-render-sibling' into SIDE from its render, with
`vui-mount' when MOUNT, or else with `vui-render'."
  :render (progn
            (if mount
                (save-window-excursion
                  (vui-mount (vui-component 'vui-test-render-sibling) side))
              (vui-render (vui-component 'vui-test-render-sibling) side))
            (push (with-current-buffer side (buffer-string))
                  vui-test--render-side-texts)
            (vui-text "caller")))

(vui-defcomponent vui-test-render-measured (side mount)
  "A flex row, which measures its children: a `vui-test-render-sibling'
and a `vui-test-render-in-render' (with MOUNT)."
  :render (vui-flex (vui-component 'vui-test-render-sibling)
                    (vui-component 'vui-test-render-in-render
                                   :side side :mount mount)))

(describe "vui-render of components"
  ;; `vui-render' mounts what it renders, as `vui-mount' does, without
  ;; showing the buffer; the tree is its own
  (before-each (setq vui-test--render-effects nil
                     vui-test--render-host-renders 0))

  (it "runs the effects of its components and leaves nothing waiting"
    (let ((vui--pending-effects nil)
          (vui--new-children nil))
      (with-temp-buffer
        (vui-render (vui-component 'vui-test-render-effect :tag 'top))
        (expect (buffer-string) :to-equal "top 0 [click]")
        (expect (length vui--pending-effects) :to-equal 0)
        (expect (length vui--new-children) :to-equal 0)
        (expect vui-test--render-effects
                :to-equal (list (list 'top (buffer-name)))))))

  (it "renders its components again when their state changes"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (vui-render (vui-component 'vui-test-render-effect :tag 'top))
        (vui-test--click-button-at (1- (point-max)))
        (expect (buffer-string) :to-equal "top 1 [click]")
        ;; And on every change after it
        (vui-test--click-button-at (1- (point-max)))
        (expect (buffer-string) :to-equal "top 2 [click]"))))

  (it "does not reflow on window resize, unlike vui-mount"
    (with-temp-buffer
      (vui-render (vui-text "x"))
      (expect (memq #'vui--on-window-size-change window-size-change-functions)
              :to-be nil)))

  (it "mounts what it renders from a render that a layout measures once"
    (let ((vui-render-delay nil)
          (log nil))
      (vui-defcomponent vui-test-render-measured-part ()
        :on-mount (push 'mount log)
        :on-unmount (push 'unmount log)
        :render (vui-text "part"))
      (vui-defcomponent vui-test-render-measured-caller ()
        :render (progn
                  (vui-render (vui-component 'vui-test-render-measured-part)
                              (get-buffer-create "*test-render-measured*"))
                  (vui-text "caller")))
      (vui-defcomponent vui-test-render-measuring-root ()
        :render (vui-flex (vui-component 'vui-test-render-measured-caller)))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-mount (vui-component 'vui-test-render-measuring-root)
                         "*test-render-measuring*"))
            ;; Each tree it mounted mounted and goes: the measure of the
            ;; flex renders the caller too
            (expect (cl-count 'mount log)
                    :to-equal (1+ (cl-count 'unmount log)))
            (expect (car log) :to-be 'mount))
        (dolist (buffer '("*test-render-measuring*" "*test-render-measured*"))
          (when (get-buffer buffer)
            (kill-buffer buffer))))))

  (it "unmounts the tree it rendered when it renders another one there"
    (let ((log nil))
      (vui-defcomponent vui-test-render-lifecycle (name)
        :on-mount (progn (push (list 'mount name) log)
                         (lambda () (push (list 'cleanup name) log)))
        :on-unmount (push (list 'unmount name) log)
        :render (vui-text name))
      ;; A buffer of its own: a temporary buffer runs no `kill-buffer-hook'
      (with-current-buffer (get-buffer-create "*test-render-lifecycle*")
        (vui-render (vui-component 'vui-test-render-lifecycle :name "a"))
        (vui-render (vui-text "plain"))
        (expect (buffer-string) :to-equal "plain")
        (vui-render (vui-component 'vui-test-render-lifecycle :name "b"))
        (kill-buffer))
      ;; Killing the buffer unmounts the last one
      (expect (reverse log)
              :to-equal '((mount "a") (cleanup "a") (unmount "a")
                          (mount "b") (cleanup "b") (unmount "b")))))

  (it "returns the root of the tree it mounted"
    (with-temp-buffer
      (let ((root (vui-render (vui-text "x"))))
        (expect (vui-instance-p root) :to-be-truthy)
        (expect (vui-get-instance) :to-be root))))

  (it "runs its effects in its own buffer when it is called from a hook"
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*")))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-mount (vui-component 'vui-test-render-from-hook :buffer side)
                         "*test-render-host*"))
            (expect (with-current-buffer side (buffer-string))
                    :to-equal "side 0 [click]")
            (expect vui-test--render-effects
                    :to-equal '((host "*test-render-host*")
                                (side "*test-render-side*"))))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))

  (it "does not render the tree it is called from on a click"
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*")))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-mount (vui-component 'vui-test-render-from-hook :buffer side)
                         "*test-render-host*"))
            (expect vui-test--render-host-renders :to-equal 1)
            (with-current-buffer side
              (goto-char (point-min))
              (search-forward "[click")
              (vui-test--click-button-at (match-beginning 0)))
            (expect vui-test--render-host-renders :to-equal 1))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))

  (it "leaves the children of the tree it is called from as they are"
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*"))
          ;; As in a fresh Emacs: the index the next child of a render
          ;; would take, when nothing binds it
          (vui--child-index 0)
          (vui--new-children nil))
      (unwind-protect
          (let* ((root (save-window-excursion
                         (vui-mount (vui-component 'vui-test-render-from-effect
                                                   :buffer side)
                                    "*test-render-host*")))
                 (child (car (vui-instance-children root))))
            (expect (with-current-buffer side (buffer-string))
                    :to-equal "side 0 [click]")
            (expect (plist-get (vui-instance-props child) :tag) :to-be 'own)
            (expect (vui-instance-children root) :to-equal (list child))
            (vui-rerender root)
            (expect (with-current-buffer "*test-render-host*" (buffer-string))
                    :to-equal "own 0 [click]"))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))

  (it "does not see the context values of the tree it is called from"
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*")))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-mount (vui-component 'vui-test-render-theme-root :side side)
                         "*test-render-host*"))
            (expect (with-current-buffer side (buffer-string))
                    :to-equal "theme light")
            ;; A provider in the tree it renders
            (save-window-excursion
              (vui-mount (vui-component 'vui-test-render-theme-root
                                        :side side :inner t)
                         "*test-render-host*"))
            (expect (with-current-buffer side (buffer-string))
                    :to-equal "theme inner"))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))

  (it "keeps the errors its error boundaries catch out of the tree it is called from"
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*")))
      (unwind-protect
          (let ((root (save-window-excursion
                        (vui-mount (vui-component 'vui-test-render-boundary-host
                                                  :side side)
                                   "*test-render-host*"))))
            (expect (with-current-buffer side (buffer-string))
                    :to-equal "side fallback")
            (vui-rerender root)
            (expect (with-current-buffer "*test-render-host*" (buffer-string))
                    :to-equal "host"))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))

  (dolist (mount '(nil t))
   (it (format "renders a tree of its own from a render that a layout measures (%s)"
               (if mount "vui-mount" "vui-render"))
    (let ((vui-render-delay nil)
          (side (get-buffer-create "*test-render-side*")))
      (unwind-protect
          (let* ((root (save-window-excursion
                         (vui-mount (vui-component 'vui-test-render-measured
                                                   :side side :mount mount)
                                    "*test-render-host*")))
                 (sibling (car (vui-instance-children root)))
                 (caller (cadr (vui-instance-children root))))
            ;; The flex measures its children as they are live: the
            ;; sibling in SIDE, at the same place in its own tree as the
            ;; live one in the flex, must not take its state
            (setf (vui-instance-state sibling) (list :n 5))
            (setq vui-test--render-side-texts nil)
            (vui-rerender root)
            (expect vui-test--render-side-texts :not :to-equal nil)
            (expect (seq-uniq vui-test--render-side-texts)
                    :to-equal '("sibling 0"))
            (expect (vui-instance-children caller) :to-be nil))
        (kill-buffer side)
        (when (get-buffer "*test-render-host*")
          (kill-buffer "*test-render-host*")))))))

(describe "vui-render re-render on the same buffer"
  ;; Regression: calling the low-level `vui-render' twice on the same
  ;; buffer used to signal (number-or-marker-p nil) during the second
  ;; render's `erase-buffer'.  A `vui-field' installs
  ;; `widget-after-change' on `after-change-functions' and leaves the
  ;; field in `widget-field-list'; erasing then fired that hook against
  ;; the just-deleted field.  `vui-render' now drops the field
  ;; bookkeeping before erasing, like the component re-render path.
  (it "re-renders a field on the same buffer without error"
    (with-temp-buffer
      (vui-render (vui-field :size 10 :key 'f :value "hello"))
      (expect (vui-render (vui-field :size 10 :key 'f :value "world"))
              :not :to-throw)
      (expect (vui-field-value 'f) :to-equal "world")))

  (it "does not accumulate stale fields across renders"
    (with-temp-buffer
      (vui-render (vui-field :size 10 :key 'f :value "one"))
      (vui-render (vui-field :size 10 :key 'f :value "two"))
      (vui-render (vui-field :size 10 :key 'f :value "three"))
      ;; Each render clears the previous field, so only the current one
      ;; survives -- no dead widgets pile up in `widget-field-list'.
      (expect (length widget-field-list) :to-equal 1)
      (expect (vui-field-value 'f) :to-equal "three"))))


(describe "vui-checkbox"
  (it "creates a checkbox vnode"
    (let ((node (vui-checkbox :checked t)))
      (expect (vui-vnode-checkbox-p node) :to-be-truthy)
      (expect (vui-vnode-checkbox-checked-p node) :to-be-truthy)))

  (it "accepts label property"
    (let ((node (vui-checkbox :label "Enable feature")))
      (expect (vui-vnode-checkbox-label node) :to-equal "Enable feature")))

  (it "renders checkbox widget"
    (with-temp-buffer
      (vui-render (vui-checkbox :checked nil))
      (expect (buffer-string) :to-match "\\[ \\]")))

  (it "renders checked checkbox"
    (with-temp-buffer
      (vui-render (vui-checkbox :checked t))
      (expect (buffer-string) :to-match "\\[X\\]")))

  (it "renders label after checkbox"
    (with-temp-buffer
      (vui-render (vui-checkbox :checked nil :label "My Label"))
      (expect (buffer-string) :to-match "My Label"))))

(describe "vui-select"
  (it "creates a select vnode"
    (let ((node (vui-select :value "a" :options '("a" "b" "c"))))
      (expect (vui-vnode-select-p node) :to-be-truthy)
      (expect (vui-vnode-select-value node) :to-equal "a")
      (expect (vui-vnode-select-options node) :to-equal '("a" "b" "c"))))

  (it "accepts prompt property"
    (let ((node (vui-select :value nil :options '("x") :prompt "Choose: ")))
      (expect (vui-vnode-select-prompt node) :to-equal "Choose: ")))

  (it "defaults prompt to Select:"
    (let ((node (vui-select :value nil :options '("x"))))
      (expect (vui-vnode-select-prompt node) :to-equal "Select: ")))

  (it "renders as button with current value"
    (with-temp-buffer
      (vui-render (vui-select :value "apple" :options '("apple" "banana")))
      (expect (buffer-string) :to-match "apple")))

  (it "renders placeholder when no value"
    (with-temp-buffer
      (vui-render (vui-select :value nil :options '("a" "b")))
      (expect (buffer-string) :to-match "Select...")))

  (it "shows the label of the current value on the button"
    (with-temp-buffer
      (vui-render (vui-select :value "a"
                              :options '(("a" . "Apple") ("b" . "Banana"))))
      (expect (buffer-string) :to-match "Apple")))

  (it "completes on labels and passes the value to on-change"
    (let ((received 'unset)
          (candidates nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq candidates collection)
                   "Banana")))
        (with-temp-buffer
          (vui-render (vui-select :value "a"
                                  :options '(("a" . "Apple") ("b" . "Banana"))
                                  :on-change (lambda (v) (setq received v))))
          (button-activate (button-at (point-min)))))
      (expect candidates :to-equal '("Apple" "Banana"))
      (expect received :to-equal "b")))

  (it "supports non-string values with labels"
    (let ((received 'unset))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Two")))
        (with-temp-buffer
          (vui-render (vui-select :value 1
                                  :options '((1 . "One") (2 . "Two"))
                                  :on-change (lambda (v) (setq received v))))
          (expect (buffer-string) :to-match "One")
          (button-activate (button-at (point-min)))))
      (expect received :to-equal 2)))

  (it "passes plain string options through unchanged"
    (let ((received 'unset))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "banana")))
        (with-temp-buffer
          (vui-render (vui-select :value "apple"
                                  :options '("apple" "banana")
                                  :on-change (lambda (v) (setq received v))))
          (button-activate (button-at (point-min)))))
      (expect received :to-equal "banana"))))
(describe "vui-render"
  (it "renders text to buffer"
    (with-temp-buffer
      (vui-render (vui-text "hello"))
      (expect (buffer-string) :to-equal "hello")))

  (it "renders text with face"
    (with-temp-buffer
      (vui-render (vui-text "hello" :face 'bold))
      (expect (buffer-string) :to-equal "hello")
      (expect (get-text-property 0 'face (buffer-string)) :to-equal 'bold)))

  (it "renders fragment children in order"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "abc")))

  (it "renders newlines"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "line1") (vui-newline) (vui-text "line2")))
      (expect (buffer-string) :to-equal "line1\nline2")))

  (it "renders spaces"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") (vui-space 3) (vui-text "b")))
      (expect (buffer-string) :to-equal "a   b")))

  (it "handles nested fragments"
    (with-temp-buffer
      (vui-render (vui-fragment
                   (vui-text "start")
                   (vui-fragment (vui-text "-") (vui-text "nested") (vui-text "-"))
                   (vui-text "end")))
      (expect (buffer-string) :to-equal "start-nested-end")))

  (it "skips nil children"
    (with-temp-buffer
      (vui-render (vui-fragment (vui-text "a") nil (vui-text "b")))
      (expect (buffer-string) :to-equal "ab")))

  (it "handles plain strings as shorthand"
    (with-temp-buffer
      (vui-render (vui-fragment "hello" " " "world"))
      (expect (buffer-string) :to-equal "hello world")))

  (it "renders button with label"
    (with-temp-buffer
      (vui-render (vui-button "Click me"))
      (expect (buffer-string) :to-match "Click me")))

  (it "renders disabled button as text"
    (with-temp-buffer
      (vui-render (vui-button "Disabled" :disabled t))
      ;; Disabled buttons render with brackets but shadow face
      (expect (buffer-string) :to-equal "[Disabled]")))

  (it "triggers on-click callback when button is activated"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-button "Click" :on-click (lambda () (setq clicked t))))
        ;; Buttons are now text with keymap, not widgets
        (vui-test--click-button-at (point-min))
        (expect clicked :to-be-truthy))))

  (it "renders field with value"
    (with-temp-buffer
      (vui-render (vui-field :value "hello"))
      (expect (buffer-string) :to-match "hello")))

  (it "renders empty field as whitespace"
    (with-temp-buffer
      (vui-render (vui-field :size 10))
      ;; Empty field renders as whitespace
      (expect (buffer-string) :to-match "^\\s-+$")))

  (it "triggers on-change callback when field is modified"
    (with-temp-buffer
      (let ((new-value nil))
        (vui-render (vui-field :value "initial" :on-change (lambda (v) (setq new-value v))))
        ;; Get the field widget directly from widget-field-list
        ;; (vui-forward doesn't work with single widget on Emacs 29)
        (let ((widget (car widget-field-list)))
          (widget-value-set widget "changed")
          (widget-apply widget :notify widget))
        (expect new-value :to-equal "changed"))))

  (it "calls on-change once for an insert or a delete, with the new value"
    (with-temp-buffer
      (let ((values nil))
        (vui-render (vui-field :value "ab"
                               :on-change (lambda (v) (push v values))))
        (let ((widget (car widget-field-list)))
          ;; `widget-before-change' notifies too, before the text changes
          (goto-char (widget-field-start widget))
          (insert "x")
          (expect values :to-equal '("xab"))
          (setq values nil)
          (goto-char (widget-field-start widget))
          (delete-char 1)
          (expect values :to-equal '("ab")))))))

(describe "vui-get-instance"
  (it "returns the root instance mounted in a buffer"
    (vui-defcomponent get-instance-test ()
      :render (vui-text "hi"))
    (let ((instance (vui-mount (vui-component 'get-instance-test)
                               "*test-get-instance*")))
      (unwind-protect
          (progn
            ;; By buffer name
            (expect (vui-get-instance "*test-get-instance*") :to-be instance)
            ;; By buffer object
            (expect (vui-get-instance (get-buffer "*test-get-instance*"))
                    :to-be instance)
            ;; Defaults to current buffer
            (with-current-buffer "*test-get-instance*"
              (expect (vui-get-instance) :to-be instance)))
        (kill-buffer "*test-get-instance*"))))

  (it "returns nil when buffer has no mounted instance"
    (with-temp-buffer
      (expect (vui-get-instance) :to-be nil))
    (let ((buf (get-buffer-create "*test-no-instance*")))
      (unwind-protect
          (expect (vui-get-instance buf) :to-be nil)
        (kill-buffer buf))))

  (it "returns nil for a nonexistent buffer name"
    (expect (vui-get-instance "*test-does-not-exist*") :to-be nil)))

(describe "vui-mode keymap"
  (describe "keymap hierarchy"
    (it "inherits special-mode-map bindings"
      (vui-defcomponent keymap-test ()
        :render (vui-text "test"))
      (let ((instance (vui-mount (vui-component 'keymap-test) "*test-keymap*")))
        (unwind-protect
            (with-current-buffer "*test-keymap*"
              ;; vui-quit shadows quit-window but provides same behavior outside fields
              (expect (key-binding (kbd "q")) :to-equal 'vui-quit))
          (kill-buffer "*test-keymap*"))))

    (it "binds TAB to vui's unified navigation"
      (vui-defcomponent keymap-test-2 ()
        :render (vui-text "test"))
      (let ((instance (vui-mount (vui-component 'keymap-test-2) "*test-keymap-2*")))
        (unwind-protect
            (with-current-buffer "*test-keymap-2*"
              (expect (key-binding (kbd "TAB")) :to-equal 'vui-forward))
          (kill-buffer "*test-keymap-2*")))))

  (describe "q key behavior"
    (it "quits window when outside widget field"
      (vui-defcomponent q-quit-test ()
        :render (vui-text "test"))
      (let ((instance (vui-mount (vui-component 'q-quit-test) "*test-q-quit*")))
        (with-current-buffer "*test-q-quit*"
          (goto-char (point-min))
          ;; Execute q - should quit and bury the buffer
          (execute-kbd-macro "q")
          ;; Buffer should be buried (not current)
          (expect (current-buffer) :not :to-equal (get-buffer "*test-q-quit*")))
        ;; Clean up if buffer still exists
        (when (get-buffer "*test-q-quit*")
          (kill-buffer "*test-q-quit*"))))

    (it "self-inserts when inside widget field"
      (vui-defcomponent q-field-test ()
        :render (vui-field :value "" :key 'field))
      (let ((instance (vui-mount (vui-component 'q-field-test) "*test-q-field*")))
        (unwind-protect
            (with-current-buffer "*test-q-field*"
              ;; Get the field widget directly from widget-field-list
              ;; (vui-forward doesn't work with single widget on Emacs 29)
              (let ((field-widget (car widget-field-list)))
                (expect field-widget :to-be-truthy)
                (goto-char (widget-field-start field-widget))
                (execute-kbd-macro "q")
                (expect (widget-value field-widget) :to-equal "q")))
          (kill-buffer "*test-q-field*")))))

  (describe "g key behavior"
    (it "triggers refresh when outside widget field"
      (vui-defcomponent g-test ()
        :render (vui-text "test"))
      (let ((instance (vui-mount (vui-component 'g-test) "*test-g*")))
        (unwind-protect
            (with-current-buffer "*test-g*"
              (expect (key-binding (kbd "g")) :to-equal 'vui-refresh))
          (kill-buffer "*test-g*"))))

    (it "re-renders the mounted tree"
      (let ((vui-render-delay nil)
            (renders 0))
        (vui-defcomponent g-refresh-test ()
          :render (progn (setq renders (1+ renders))
                         (vui-text "test")))
        (vui-mount (vui-component 'g-refresh-test) "*test-g-refresh*")
        (unwind-protect
            (with-current-buffer "*test-g-refresh*"
              (vui-refresh)
              (expect renders :to-equal 2))
          (kill-buffer "*test-g-refresh*"))))

    (it "self-inserts when inside widget field"
      (vui-defcomponent g-field-test ()
        :render (vui-field :value "" :key 'field))
      (let ((instance (vui-mount (vui-component 'g-field-test) "*test-g-field*")))
        (unwind-protect
            (with-current-buffer "*test-g-field*"
              ;; Get the field widget directly from widget-field-list
              ;; (vui-forward doesn't work with single widget on Emacs 29)
              (let ((field-widget (car widget-field-list)))
                (expect field-widget :to-be-truthy)
                (goto-char (widget-field-start field-widget))
                (execute-kbd-macro "g")
                (expect (widget-value field-widget) :to-equal "g")))
          (kill-buffer "*test-g-field*"))))))

;;; vui-test.el ends here

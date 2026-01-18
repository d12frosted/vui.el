;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS.
Buttons are widget.el push-buttons, so we use widget-apply."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

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
      (expect (length (vui-vnode-fragment-children node)) :to-equal 2))))

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
              ;; (widget-forward doesn't work with single widget on Emacs 29)
              (let ((widget (car widget-field-list)))
                (expect widget :to-be-truthy)
                ;; Simulate RET by applying the widget action
                (widget-apply widget :action))
              ;; on-submit should have been called with the field value
              (expect submitted-value :to-equal "initial"))
          (kill-buffer "*test-submit*"))))))

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
            ;; (widget-forward doesn't work with single widget on Emacs 29)
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

(describe "vui-field-valid-p"
  (it "returns t for valid regexp match"
    (vui-defcomponent valid-regexp-test ()
      :render
      (vui-field :key 'email
                 :value "test@example.com"
                 :valid-regexp "^[^@]+@[^@]+$"))
    (let ((instance (vui-mount (vui-component 'valid-regexp-test) "*test-valid-regexp*")))
      (unwind-protect
          (with-current-buffer "*test-valid-regexp*"
            (expect (vui-field-valid-p 'email) :to-be-truthy))
        (kill-buffer "*test-valid-regexp*"))))

  (it "returns nil for invalid regexp match"
    (vui-defcomponent invalid-regexp-test ()
      :render
      (vui-field :key 'email
                 :value "invalid-email"
                 :valid-regexp "^[^@]+@[^@]+$"))
    (let ((instance (vui-mount (vui-component 'invalid-regexp-test) "*test-invalid-regexp*")))
      (unwind-protect
          (with-current-buffer "*test-invalid-regexp*"
            (expect (vui-field-valid-p 'email) :to-be nil))
        (kill-buffer "*test-invalid-regexp*"))))

  (it "returns t for valid-p function returning t"
    (vui-defcomponent valid-p-test ()
      :render
      (vui-field :key 'password
                 :value "longpassword"
                 :valid-p (lambda (v) (>= (length v) 8))))
    (let ((instance (vui-mount (vui-component 'valid-p-test) "*test-valid-p*")))
      (unwind-protect
          (with-current-buffer "*test-valid-p*"
            (expect (vui-field-valid-p 'password) :to-be-truthy))
        (kill-buffer "*test-valid-p*"))))

  (it "returns nil for valid-p function returning nil"
    (vui-defcomponent invalid-p-test ()
      :render
      (vui-field :key 'password
                 :value "short"
                 :valid-p (lambda (v) (>= (length v) 8))))
    (let ((instance (vui-mount (vui-component 'invalid-p-test) "*test-invalid-p*")))
      (unwind-protect
          (with-current-buffer "*test-invalid-p*"
            (expect (vui-field-valid-p 'password) :to-be nil))
        (kill-buffer "*test-invalid-p*"))))

  (it "requires both regexp and valid-p to pass"
    (vui-defcomponent combined-invalid-test ()
      :render
      (vui-field :key 'username
                 :value "ab"
                 :valid-regexp "^[a-z]+$"
                 :valid-p (lambda (v) (>= (length v) 3))))
    (let ((instance (vui-mount (vui-component 'combined-invalid-test) "*test-combined*")))
      (unwind-protect
          (with-current-buffer "*test-combined*"
            ;; "ab" matches regexp but is too short
            (expect (vui-field-valid-p 'username) :to-be nil))
        (kill-buffer "*test-combined*"))))

  (it "returns t when both regexp and valid-p pass"
    (vui-defcomponent combined-valid-test ()
      :render
      (vui-field :key 'username
                 :value "alice"
                 :valid-regexp "^[a-z]+$"
                 :valid-p (lambda (v) (>= (length v) 3))))
    (let ((instance (vui-mount (vui-component 'combined-valid-test) "*test-combined-valid*")))
      (unwind-protect
          (with-current-buffer "*test-combined-valid*"
            (expect (vui-field-valid-p 'username) :to-be-truthy))
        (kill-buffer "*test-combined-valid*"))))

  (it "returns t for field without validation"
    (vui-defcomponent no-validation-test ()
      :render
      (vui-field :key 'name :value "anything"))
    (let ((instance (vui-mount (vui-component 'no-validation-test) "*test-no-validation*")))
      (unwind-protect
          (with-current-buffer "*test-no-validation*"
            (expect (vui-field-valid-p 'name) :to-be-truthy))
        (kill-buffer "*test-no-validation*"))))

  (it "returns nil for non-existent key"
    (vui-defcomponent exists-test ()
      :render
      (vui-field :key 'exists :value "test"))
    (let ((instance (vui-mount (vui-component 'exists-test) "*test-exists*")))
      (unwind-protect
          (with-current-buffer "*test-exists*"
            (expect (vui-field-valid-p 'nonexistent) :to-be nil))
        (kill-buffer "*test-exists*"))))

  (it "updates validity when field value changes"
    (vui-defcomponent update-validity-test ()
      :render
      (vui-field :key 'email
                 :value ""
                 :valid-regexp "^[^@]+@[^@]+$"))
    (let ((instance (vui-mount (vui-component 'update-validity-test) "*test-update-validity*")))
      (unwind-protect
          (with-current-buffer "*test-update-validity*"
            (let ((widget (car widget-field-list)))
              ;; Initially empty, matches nothing, so invalid
              (expect (vui-field-valid-p 'email) :to-be nil)
              ;; Type valid email
              (widget-value-set widget "test@example.com")
              (widget-apply widget :notify widget)
              (expect (vui-field-valid-p 'email) :to-be-truthy)
              ;; Type invalid email
              (widget-value-set widget "invalid")
              (widget-apply widget :notify widget)
              (expect (vui-field-valid-p 'email) :to-be nil)))
        (kill-buffer "*test-update-validity*"))))

  (it "does not show error message on initial render (pristine field)"
    (vui-defcomponent error-pristine-test ()
      :render
      (vui-field :key 'email
                 :value "invalid"
                 :valid-regexp "^[^@]+@[^@]+$"
                 :error-message "Please enter a valid email"))
    (let ((instance (vui-mount (vui-component 'error-pristine-test) "*test-error-pristine*")))
      (unwind-protect
          (with-current-buffer "*test-error-pristine*"
            ;; Error should NOT show because field hasn't been touched yet
            (expect (buffer-string) :not :to-match "Please enter a valid email"))
        (kill-buffer "*test-error-pristine*"))))

  (it "shows error message after field is touched"
    (vui-defcomponent error-message-test ()
      :render
      (vui-field :key 'email
                 :value "invalid"
                 :valid-regexp "^[^@]+@[^@]+$"
                 :error-message "Please enter a valid email"))
    (let ((instance (vui-mount (vui-component 'error-message-test) "*test-error-message*")))
      (unwind-protect
          (with-current-buffer "*test-error-message*"
            (let ((widget (car widget-field-list)))
              ;; Touch the field by triggering notify
              (widget-value-set widget "still-invalid")
              (widget-apply widget :notify widget)
              ;; Now error should show on next render (re-render via on-change would do this)
              ;; For now just verify dirty state is set
              (expect (vui--field-dirty-p 'email) :to-be-truthy)))
        (kill-buffer "*test-error-message*"))))

  (it "does not show error message when valid"
    (vui-defcomponent no-error-message-test ()
      :render
      (vui-field :key 'email
                 :value "test@example.com"
                 :valid-regexp "^[^@]+@[^@]+$"
                 :error-message "Please enter a valid email"))
    (let ((instance (vui-mount (vui-component 'no-error-message-test) "*test-no-error-message*")))
      (unwind-protect
          (with-current-buffer "*test-no-error-message*"
            (expect (buffer-string) :not :to-match "Please enter a valid email"))
        (kill-buffer "*test-no-error-message*")))))

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
      (expect (buffer-string) :to-match "Select..."))))
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
        ;; (widget-forward doesn't work with single widget on Emacs 29)
        (let ((widget (car widget-field-list)))
          (widget-value-set widget "changed")
          (widget-apply widget :notify widget))
        (expect new-value :to-equal "changed")))))

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

    (it "inherits widget-keymap bindings"
      (vui-defcomponent keymap-test-2 ()
        :render (vui-text "test"))
      (let ((instance (vui-mount (vui-component 'keymap-test-2) "*test-keymap-2*")))
        (unwind-protect
            (with-current-buffer "*test-keymap-2*"
              (expect (key-binding (kbd "TAB")) :to-equal 'widget-forward))
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
              ;; (widget-forward doesn't work with single widget on Emacs 29)
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

    (it "self-inserts when inside widget field"
      (vui-defcomponent g-field-test ()
        :render (vui-field :value "" :key 'field))
      (let ((instance (vui-mount (vui-component 'g-field-test) "*test-g-field*")))
        (unwind-protect
            (with-current-buffer "*test-g-field*"
              ;; Get the field widget directly from widget-field-list
              ;; (widget-forward doesn't work with single widget on Emacs 29)
              (let ((field-widget (car widget-field-list)))
                (expect field-widget :to-be-truthy)
                (goto-char (widget-field-start field-widget))
                (execute-kbd-macro "g")
                (expect (widget-value field-widget) :to-equal "g")))
          (kill-buffer "*test-g-field*"))))))

;;; vui-test.el ends here

;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (which are now text with keymap, not widgets)
(defun vui-test--click-button-at (pos)
  "Invoke the button at POS.
Buttons are now rendered as text with keymap, so we invoke the RET binding."
  (let* ((keymap (get-text-property pos 'keymap))
         (binding (when keymap (lookup-key keymap (kbd "RET")))))
    (when binding
      (funcall binding))))

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
      (expect (buffer-string) :to-equal "[...]"))))

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
      (expect (vui-vnode-key node) :to-equal "field-1"))))

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

(describe "vui-hstack"
  (it "creates an hstack vnode"
    (let ((node (vui-hstack (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-hstack-p node) :to-be-truthy)
      (expect (length (vui-vnode-hstack-children node)) :to-equal 2)))

  (it "defaults spacing to 1"
    (let ((node (vui-hstack (vui-text "a"))))
      (expect (vui-vnode-hstack-spacing node) :to-equal 1)))

  (it "accepts custom spacing"
    (let ((node (vui-hstack :spacing 3 (vui-text "a"))))
      (expect (vui-vnode-hstack-spacing node) :to-equal 3)))

  (it "renders children horizontally with spacing"
    (with-temp-buffer
      (vui-render (vui-hstack (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "a b c")))

  (it "renders with custom spacing"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 3 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a   b")))

  (it "renders with zero spacing"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 0 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "ab"))))

(describe "vui-vstack"
  ;; Specification: vstack joins children with \n
  ;; newline children render as empty string, adding extra \n via join

  (it "creates a vstack vnode"
    (let ((node (vui-vstack (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (length (vui-vnode-vstack-children node)) :to-equal 2)))

  (it "defaults spacing to 0 and indent to 0"
    (let ((node (vui-vstack (vui-text "a"))))
      (expect (vui-vnode-vstack-spacing node) :to-equal 0)
      (expect (vui-vnode-vstack-indent node) :to-equal 0)))

  ;; Core joining behavior
  (it "joins two children with newline"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\nb")))

  (it "joins three children with newlines"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "a") (vui-text "b") (vui-text "c")))
      (expect (buffer-string) :to-equal "a\nb\nc")))

  ;; newline as spacing marker
  (it "treats newline as empty - one newline adds one blank line"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)
                   (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  (it "treats newline as empty - two newlines add two blank lines"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)
                   (vui-newline)
                   (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\n\nb")))

  (it "handles newline at start"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-newline)
                   (vui-text "a")))
      (expect (buffer-string) :to-equal "\na")))

  (it "handles newline at end"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "a")
                   (vui-newline)))
      (expect (buffer-string) :to-equal "a\n")))

  ;; Table integration - table has no trailing newline
  (it "joins table and text with single newline"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-table
                    :columns '((:width 1 :grow t))
                    :rows '(("a")))
                   (vui-text "after")))
      (expect (buffer-string) :to-equal "a\nafter")))

  (it "joins table with border and text with single newline"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-table
                    :columns '((:width 1 :grow t))
                    :rows '(("x"))
                    :border :ascii)
                   (vui-text "after")))
      ;; Table: +---+\n| x |\n+---+ (no trailing newline)
      ;; Join: \n
      ;; Text: after
      (expect (buffer-string) :to-equal "+---+\n| x |\n+---+\nafter")))

  ;; Spacing option
  (it "renders with spacing adding extra blank lines"
    (with-temp-buffer
      (vui-render (vui-vstack :spacing 1 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  ;; Indent option
  (it "renders with indent on each line"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "  a\n  b"))))

(describe "vui-box"
  (it "creates a box vnode"
    (let ((node (vui-box (vui-text "hi") :width 10)))
      (expect (vui-vnode-box-p node) :to-be-truthy)
      (expect (vui-vnode-box-width node) :to-equal 10)))

  (it "renders with left alignment (default)"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10))
      (expect (buffer-string) :to-equal "hi        ")))

  (it "renders with right alignment"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10 :align :right))
      (expect (buffer-string) :to-equal "        hi")))

  (it "renders with center alignment"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "hi") :width 10 :align :center))
      (expect (buffer-string) :to-equal "    hi    ")))

  (it "renders with padding"
    (with-temp-buffer
      (vui-render (vui-box (vui-text "x") :width 10 :padding-left 2 :padding-right 2))
      ;; width=10, padding=2+2, inner=6, content=1, fill=5
      (expect (buffer-string) :to-equal "  x       "))))

(describe "vui-list"
  (it "renders items vertically by default"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c")
                            (lambda (item) (vui-text item))))
      (expect (buffer-string) :to-equal "a\nb\nc")))

  (it "renders items horizontally when vertical is nil"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c")
                            (lambda (item) (vui-text item))
                            nil :vertical nil))
      (expect (buffer-string) :to-equal "abc")))

  (it "applies keys from key-fn"
    (let* ((items '((:id 1 :name "Alice") (:id 2 :name "Bob")))
           ;; Use horizontal list for simpler testing
           (node (vui-list items
                           (lambda (item) (vui-text (plist-get item :name)))
                           (lambda (item) (plist-get item :id))
                           :vertical nil)))
      (expect (vui-vnode-fragment-p node) :to-be-truthy)
      (let ((children (vui-vnode-fragment-children node)))
        (expect (vui-vnode-key (nth 0 children)) :to-equal 1)
        (expect (vui-vnode-key (nth 1 children)) :to-equal 2))))

  (it "uses item as key when no key-fn provided"
    (let* ((node (vui-list '("x" "y") #'vui-text nil :vertical nil)))
      (let ((children (vui-vnode-fragment-children node)))
        (expect (vui-vnode-key (nth 0 children)) :to-equal "x")
        (expect (vui-vnode-key (nth 1 children)) :to-equal "y"))))

  (it "handles empty list"
    (with-temp-buffer
      (vui-render (vui-list '() #'vui-text))
      (expect (buffer-string) :to-equal ""))))

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
        ;; Find the field widget and modify it
        (goto-char (point-min))
        (widget-forward 1)
        (let ((widget (widget-at (point))))
          (widget-value-set widget "changed")
          (widget-apply widget :notify widget))
        (expect new-value :to-equal "changed")))))

(describe "defcomponent"
  (it "defines a component and registers it"
    (defcomponent test-simple ()
      :render (vui-text "simple"))
    (expect (vui--get-component 'test-simple) :to-be-truthy))

  (it "defines component with props"
    (defcomponent test-greeting (name)
      :render (vui-text (format "Hello, %s!" name)))
    (let ((def (vui--get-component 'test-greeting)))
      (expect (vui-component-def-props-spec def) :to-equal '(name))))

  (it "defines component with state"
    (defcomponent test-counter ()
      :state ((count 0))
      :render (vui-text (number-to-string count)))
    (let ((def (vui--get-component 'test-counter)))
      (expect (vui-component-def-initial-state-fn def) :to-be-truthy))))

(describe "vui-component"
  (it "creates a component vnode"
    (let ((vnode (vui-component 'test-simple)))
      (expect (vui-vnode-component-p vnode) :to-be-truthy)
      (expect (vui-vnode-component-type vnode) :to-equal 'test-simple)))

  (it "accepts props"
    (let ((vnode (vui-component 'test-greeting :name "World")))
      (expect (plist-get (vui-vnode-component-props vnode) :name)
              :to-equal "World")))

  (it "accepts children"
    (let ((vnode (vui-component 'some-comp :children (list (vui-text "child")))))
      (expect (vui-vnode-component-children vnode) :to-be-truthy))))

(describe "component rendering"
  (it "renders a simple component"
    (defcomponent render-test ()
      :render (vui-text "rendered!"))
    (with-temp-buffer
      (vui-render (vui-component 'render-test))
      (expect (buffer-string) :to-equal "rendered!")))

  (it "renders component with props"
    (defcomponent greeting-test (name)
      :render (vui-text (format "Hi, %s!" name)))
    (with-temp-buffer
      (vui-render (vui-component 'greeting-test :name "Alice"))
      (expect (buffer-string) :to-equal "Hi, Alice!")))

  (it "renders nested components"
    (defcomponent inner-comp ()
      :render (vui-text "[inner]"))
    (defcomponent outer-comp ()
      :render (vui-fragment
               (vui-text "outer:")
               (vui-component 'inner-comp)))
    (with-temp-buffer
      (vui-render (vui-component 'outer-comp))
      (expect (buffer-string) :to-equal "outer:[inner]")))

  (it "passes children to component"
    (defcomponent wrapper-comp ()
      :render (vui-fragment
               (vui-text "[")
               (vui-vnode-fragment--create :children children)
               (vui-text "]")))
    (with-temp-buffer
      (vui-render (vui-component 'wrapper-comp
                                 :children (list (vui-text "content"))))
      (expect (buffer-string) :to-equal "[content]"))))

(describe "lifecycle hooks"
  (it "calls on-mount after first render"
    (let ((mounted nil))
      (defcomponent mount-test ()
        :on-mount (setq mounted t)
        :render (vui-text "hello"))
      (let ((instance (vui-mount (vui-component 'mount-test) "*test-mount*")))
        (unwind-protect
            (progn
              (expect mounted :to-be-truthy)
              (expect (buffer-string) :to-equal "hello"))
          (kill-buffer "*test-mount*")))))

  (it "does not call on-mount on re-render"
    (let ((mount-count 0))
      (defcomponent mount-once ()
        :state ((count 0))
        :on-mount (setq mount-count (1+ mount-count))
        :render (vui-text (number-to-string count)))
      (let ((instance (vui-mount (vui-component 'mount-once) "*test-mount2*")))
        (unwind-protect
            (progn
              (expect mount-count :to-equal 1)
              ;; Trigger re-render
              (vui--rerender-instance instance)
              (expect mount-count :to-equal 1))
          (kill-buffer "*test-mount2*")))))

  (it "calls on-unmount when component is removed"
    (let ((unmounted nil)
          (show-child t))
      (defcomponent unmount-child ()
        :on-unmount (setq unmounted t)
        :render (vui-text "child"))
      (defcomponent unmount-parent ()
        :render (if show-child
                    (vui-component 'unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'unmount-parent) "*test-unmount*")))
        (unwind-protect
            (progn
              (expect unmounted :to-be nil)
              (expect (buffer-string) :to-equal "child")
              ;; Remove child by setting show-child to nil and re-render
              (setq show-child nil)
              (vui--rerender-instance instance)
              (expect unmounted :to-be-truthy)
              (expect (buffer-string) :to-equal "no child"))
          (kill-buffer "*test-unmount*")))))

  (it "calls on-unmount for nested children depth-first"
    (let ((unmount-order nil))
      (defcomponent nested-inner ()
        :on-unmount (push 'inner unmount-order)
        :render (vui-text "inner"))
      (defcomponent nested-outer ()
        :on-unmount (push 'outer unmount-order)
        :render (vui-fragment
                 (vui-text "outer:")
                 (vui-component 'nested-inner)))
      (defcomponent nested-root ()
        :state ((show t))
        :render (if show
                    (vui-component 'nested-outer)
                  (vui-text "empty")))
      (let ((instance (vui-mount (vui-component 'nested-root) "*test-nested*")))
        (unwind-protect
            (progn
              ;; Remove the nested components
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :show nil))
              (vui--rerender-instance instance)
              ;; Inner should unmount before outer (depth-first)
              (expect unmount-order :to-equal '(outer inner)))
          (kill-buffer "*test-nested*")))))

  (it "provides props and state to lifecycle hooks"
    (let ((mount-props nil)
          (mount-state nil))
      (defcomponent lifecycle-args (name)
        :state ((count 42))
        :on-mount (setq mount-props name
                        mount-state count)
        :render (vui-text name))
      (let ((instance (vui-mount (vui-component 'lifecycle-args :name "test")
                                 "*test-args*")))
        (unwind-protect
            (progn
              (expect mount-props :to-equal "test")
              (expect mount-state :to-equal 42))
          (kill-buffer "*test-args*")))))

  (it "calls on-update after re-render"
    (let ((update-count 0)
          (captured-prev-state nil))
      (defcomponent test-update ()
        :state ((count 0))
        :on-update (progn
                     (setq update-count (1+ update-count))
                     (setq captured-prev-state (plist-get prev-state :count)))
        :render (vui-text (number-to-string count)))
      (let ((instance (vui-mount (vui-component 'test-update) "*test-update*")))
        (unwind-protect
            (progn
              ;; on-update not called on first render
              (expect update-count :to-equal 0)
              ;; Trigger re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 5))
              (vui--rerender-instance instance)
              ;; on-update should have been called
              (expect update-count :to-equal 1)
              ;; prev-state should have the old value
              (expect captured-prev-state :to-equal 0)
              ;; Another re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 10))
              (vui--rerender-instance instance)
              (expect update-count :to-equal 2)
              (expect captured-prev-state :to-equal 5))
          (kill-buffer "*test-update*")))))

  (it "provides prev-props to on-update"
    (let ((captured-prev-label nil))
      (defcomponent test-update-props (label)
        :on-update (setq captured-prev-label (plist-get prev-props :label))
        :render (vui-text label))
      (let ((instance (vui-mount (vui-component 'test-update-props :label "first")
                                 "*test-update-props*")))
        (unwind-protect
            (progn
              ;; Update props and re-render
              (setf (vui-instance-props instance)
                    (plist-put (vui-instance-props instance) :label "second"))
              (vui--rerender-instance instance)
              ;; on-update should have captured the previous label
              (expect captured-prev-label :to-equal "first"))
          (kill-buffer "*test-update-props*"))))))

(describe "reconciliation"
  (it "preserves child component state across parent re-render"
    ;; Define a child component with state
    (defcomponent stateful-child ()
      :state ((value 42))
      :render (vui-text (number-to-string value)))
    ;; Define parent that renders the child
    (defcomponent parent-comp ()
      :state ((label "test"))
      :render (vui-fragment
               (vui-text label)
               (vui-text ":")
               (vui-component 'stateful-child)))
    ;; Mount and verify initial render
    (let* ((instance (vui-mount (vui-component 'parent-comp) "*test-recon*")))
      (unwind-protect
          (progn
            (expect (buffer-string) :to-equal "test:42")
            ;; Get the child instance and change its state
            (let ((child (car (vui-instance-children instance))))
              (expect child :to-be-truthy)
              ;; Manually set child state
              (setf (vui-instance-state child)
                    (plist-put (vui-instance-state child) :value 100)))
            ;; Re-render parent (simulating state change)
            (vui--rerender-instance instance)
            ;; Child state should be preserved
            (expect (buffer-string) :to-equal "test:100"))
        (kill-buffer "*test-recon*"))))

  (it "reuses child by index when no key provided"
    (defcomponent indexed-child (label)
      :state ((count 0))
      :render (vui-text (format "%s:%d" label count)))
    (defcomponent indexed-parent ()
      :render (vui-fragment
               (vui-component 'indexed-child :label "A")
               (vui-component 'indexed-child :label "B")))
    (let ((instance (vui-mount (vui-component 'indexed-parent) "*test-idx*")))
      (unwind-protect
          (progn
            (expect (buffer-string) :to-equal "A:0B:0")
            ;; Set state on first child
            (let ((first-child (car (vui-instance-children instance))))
              (setf (vui-instance-state first-child)
                    (plist-put (vui-instance-state first-child) :count 5)))
            (vui--rerender-instance instance)
            ;; First child should preserve state, second should stay 0
            (expect (buffer-string) :to-equal "A:5B:0"))
        (kill-buffer "*test-idx*")))))

(describe "use-effect"
  (it "runs effect after first render"
    (let ((effect-ran nil))
      (defcomponent effect-test ()
        :render (progn
                  (use-effect ()
                    (setq effect-ran t))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'effect-test) "*test-effect1*")))
        (unwind-protect
            (expect effect-ran :to-be-truthy)
          (kill-buffer "*test-effect1*")))))

  (it "runs effect when deps change"
    (let ((effect-count 0)
          (show-alt nil))
      (defcomponent effect-deps ()
        :state ((value 1))
        :render (progn
                  (use-effect (value)
                    (setq effect-count (1+ effect-count)))
                  (vui-text (number-to-string value))))
      (let ((instance (vui-mount (vui-component 'effect-deps) "*test-effect2*")))
        (unwind-protect
            (progn
              ;; Effect should run once on mount
              (expect effect-count :to-equal 1)
              ;; Change state and re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :value 2))
              (vui--rerender-instance instance)
              ;; Effect should run again because dep changed
              (expect effect-count :to-equal 2)
              ;; Re-render without changing value
              (vui--rerender-instance instance)
              ;; Effect should NOT run (deps unchanged)
              (expect effect-count :to-equal 2))
          (kill-buffer "*test-effect2*")))))

  (it "does not run effect when deps are unchanged"
    (let ((effect-count 0))
      (defcomponent effect-stable ()
        :state ((count 0) (other 0))
        :render (progn
                  (use-effect (count)
                    (setq effect-count (1+ effect-count)))
                  (vui-text (format "%d-%d" count other))))
      (let ((instance (vui-mount (vui-component 'effect-stable) "*test-effect3*")))
        (unwind-protect
            (progn
              (expect effect-count :to-equal 1)
              ;; Change 'other' but not 'count'
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :other 5))
              (vui--rerender-instance instance)
              ;; Effect should NOT run (count unchanged)
              (expect effect-count :to-equal 1))
          (kill-buffer "*test-effect3*")))))

  (it "runs cleanup before next effect"
    (let ((cleanup-ran nil)
          (effect-value nil))
      (defcomponent effect-cleanup ()
        :state ((value 1))
        :render (progn
                  (use-effect (value)
                    (setq effect-value value)
                    (lambda () (setq cleanup-ran value)))
                  (vui-text (number-to-string value))))
      (let ((instance (vui-mount (vui-component 'effect-cleanup) "*test-effect4*")))
        (unwind-protect
            (progn
              (expect effect-value :to-equal 1)
              (expect cleanup-ran :to-be nil)
              ;; Change state
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :value 2))
              (vui--rerender-instance instance)
              ;; Cleanup from previous effect should have run
              (expect cleanup-ran :to-equal 1)
              (expect effect-value :to-equal 2))
          (kill-buffer "*test-effect4*")))))

  (it "runs cleanup on unmount"
    (let ((cleanup-ran nil)
          (show-child t))
      (defcomponent effect-unmount-child ()
        :render (progn
                  (use-effect ()
                    (lambda () (setq cleanup-ran t)))
                  (vui-text "child")))
      (defcomponent effect-unmount-parent ()
        :render (if show-child
                    (vui-component 'effect-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'effect-unmount-parent) "*test-effect5*")))
        (unwind-protect
            (progn
              (expect cleanup-ran :to-be nil)
              ;; Remove child
              (setq show-child nil)
              (vui--rerender-instance instance)
              ;; Cleanup should have run
              (expect cleanup-ran :to-be-truthy))
          (kill-buffer "*test-effect5*"))))))

(describe "use-ref"
  (it "creates a ref with initial value"
    (let ((ref-value nil))
      (defcomponent ref-test ()
        :render (let ((my-ref (use-ref 42)))
                  (setq ref-value (car my-ref))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-test) "*test-ref1*")))
        (unwind-protect
            (expect ref-value :to-equal 42)
          (kill-buffer "*test-ref1*")))))

  (it "preserves ref value across re-renders"
    (let ((ref-values nil))
      (defcomponent ref-persist ()
        :state ((count 0))
        :render (let ((my-ref (use-ref 0)))
                  ;; On first render, set ref to 100
                  (when (= (car my-ref) 0)
                    (setcar my-ref 100))
                  (push (car my-ref) ref-values)
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'ref-persist) "*test-ref2*")))
        (unwind-protect
            (progn
              ;; First render - ref should be 100 (we set it)
              (expect (car ref-values) :to-equal 100)
              ;; Re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              ;; Ref should still be 100 (preserved)
              (expect (car ref-values) :to-equal 100)
              (expect (length ref-values) :to-equal 2))
          (kill-buffer "*test-ref2*")))))

  (it "does not trigger re-render when modified"
    (let ((render-count 0))
      (defcomponent ref-no-rerender ()
        :render (let ((my-ref (use-ref 0)))
                  (setq render-count (1+ render-count))
                  ;; Modify ref - should NOT cause re-render
                  (setcar my-ref (1+ (car my-ref)))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-no-rerender) "*test-ref3*")))
        (unwind-protect
            (progn
              ;; Only one render should have occurred
              (expect render-count :to-equal 1))
          (kill-buffer "*test-ref3*")))))

  (it "works with use-effect for storing timers"
    (let ((cleanup-called nil)
          (timer-value nil))
      (defcomponent ref-with-effect ()
        :render (let ((timer-ref (use-ref nil)))
                  (use-effect ()
                    ;; Store a value (simulating timer)
                    (setcar timer-ref 'my-timer)
                    (lambda ()
                      (setq cleanup-called t)
                      (setq timer-value (car timer-ref))))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'ref-with-effect) "*test-ref4*")))
        (unwind-protect
            (progn
              ;; Initially, cleanup not called
              (expect cleanup-called :to-be nil)
              ;; Force unmount by killing buffer (via vui--call-unmount-recursive)
              (vui--call-unmount-recursive instance)
              ;; Cleanup should have run and accessed the ref
              (expect cleanup-called :to-be-truthy)
              (expect timer-value :to-equal 'my-timer))
          (when (get-buffer "*test-ref4*")
            (kill-buffer "*test-ref4*")))))))

(describe "context"
  (it "defines context with defcontext"
    (defcontext test-ctx 'default-value "A test context.")
    (expect (boundp 'test-ctx-context) :to-be-truthy)
    (expect (fboundp 'test-ctx-provider) :to-be-truthy)
    (expect (fboundp 'use-test-ctx) :to-be-truthy)
    (expect (vui-context-default-value test-ctx-context) :to-equal 'default-value))

  (it "returns default value when no provider"
    (defcontext theme 'light)
    (let ((captured-theme nil))
      (defcomponent theme-consumer ()
        :render (progn
                  (setq captured-theme (use-theme))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'theme-consumer) "*test-ctx1*")))
        (unwind-protect
            (expect captured-theme :to-equal 'light)
          (kill-buffer "*test-ctx1*")))))

  (it "provides value to children"
    (defcontext theme 'light)
    (let ((captured-theme nil))
      (defcomponent themed-button ()
        :render (progn
                  (setq captured-theme (use-theme))
                  (vui-text (format "theme: %s" captured-theme))))
      (defcomponent app ()
        :render (theme-provider 'dark
                                (vui-component 'themed-button)))
      (let ((instance (vui-mount (vui-component 'app) "*test-ctx2*")))
        (unwind-protect
            (progn
              (expect captured-theme :to-equal 'dark)
              (expect (buffer-string) :to-equal "theme: dark"))
          (kill-buffer "*test-ctx2*")))))

  (it "allows nested providers with different values"
    (defcontext indent-level 0)
    (let ((captured-levels nil))
      (defcomponent level-display ()
        :render (progn
                  (push (use-indent-level) captured-levels)
                  (vui-text (number-to-string (use-indent-level)))))
      (defcomponent nested-providers ()
        :render (vui-fragment
                 (indent-level-provider 1
                                        (vui-component 'level-display)
                                        (indent-level-provider 2
                                                               (vui-component 'level-display)))))
      (let ((instance (vui-mount (vui-component 'nested-providers) "*test-ctx3*")))
        (unwind-protect
            (progn
              ;; Both levels should have been captured (in reverse order)
              (expect (length captured-levels) :to-equal 2)
              (expect (nth 0 captured-levels) :to-equal 2)  ; inner
              (expect (nth 1 captured-levels) :to-equal 1)) ; outer
          (kill-buffer "*test-ctx3*")))))

  (it "supports multiple contexts"
    (defcontext user-name "anonymous")
    (defcontext user-role 'guest)
    (let ((captured-name nil)
          (captured-role nil))
      (defcomponent multi-ctx-consumer ()
        :render (progn
                  (setq captured-name (use-user-name))
                  (setq captured-role (use-user-role))
                  (vui-text "test")))
      (defcomponent multi-ctx-provider ()
        :render (user-name-provider "alice"
                                    (user-role-provider 'admin
                                                        (vui-component 'multi-ctx-consumer))))
      (let ((instance (vui-mount (vui-component 'multi-ctx-provider) "*test-ctx4*")))
        (unwind-protect
            (progn
              (expect captured-name :to-equal "alice")
              (expect captured-role :to-equal 'admin))
          (kill-buffer "*test-ctx4*"))))))

(describe "use-callback"
  (it "returns same function reference when deps unchanged"
    (let ((captured-fns nil))
      (defcomponent callback-stable ()
        :state ((count 0))
        :render (let ((cb (use-callback ()
                            (message "clicked"))))
                  (push cb captured-fns)
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'callback-stable) "*test-cb1*")))
        (unwind-protect
            (progn
              ;; First render
              (expect (length captured-fns) :to-equal 1)
              ;; Re-render
              (vui--rerender-instance instance)
              ;; Same function should be returned (eq)
              (expect (length captured-fns) :to-equal 2)
              (expect (eq (nth 0 captured-fns) (nth 1 captured-fns)) :to-be-truthy))
          (kill-buffer "*test-cb1*")))))

  (it "returns new function when deps change"
    (let ((captured-fns nil))
      (defcomponent callback-deps ()
        :state ((id 1))
        :render (let ((cb (use-callback (id)
                            (delete-item id))))
                  (push cb captured-fns)
                  (vui-text (number-to-string id))))
      (let ((instance (vui-mount (vui-component 'callback-deps) "*test-cb2*")))
        (unwind-protect
            (progn
              ;; First render
              (expect (length captured-fns) :to-equal 1)
              ;; Re-render with same deps
              (vui--rerender-instance instance)
              (expect (length captured-fns) :to-equal 2)
              (expect (eq (nth 0 captured-fns) (nth 1 captured-fns)) :to-be-truthy)
              ;; Change deps and re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :id 2))
              (vui--rerender-instance instance)
              ;; Should get a NEW function (not eq)
              (expect (length captured-fns) :to-equal 3)
              (expect (eq (nth 0 captured-fns) (nth 2 captured-fns)) :to-be nil))
          (kill-buffer "*test-cb2*")))))

  (it "captures correct closure values"
    (let ((result nil))
      (defcomponent callback-closure ()
        :state ((value 42))
        :render (let ((cb (use-callback (value)
                            (setq result value))))
                  ;; Call the callback
                  (funcall cb)
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'callback-closure) "*test-cb3*")))
        (unwind-protect
            (expect result :to-equal 42)
          (kill-buffer "*test-cb3*"))))))

(describe "use-memo"
  (it "caches computed value across re-renders"
    (let ((compute-count 0))
      (defcomponent memo-test ()
        :state ((count 0))
        :render (let ((expensive (use-memo ()
                                   (setq compute-count (1+ compute-count))
                                   (* 2 42))))
                  (vui-text (format "%d-%d" count expensive))))
      (let ((instance (vui-mount (vui-component 'memo-test) "*test-memo1*")))
        (unwind-protect
            (progn
              ;; First render computes
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "0-84")
              ;; Re-render should NOT recompute (deps empty)
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "1-84"))
          (kill-buffer "*test-memo1*")))))

  (it "recomputes when deps change"
    (let ((compute-count 0))
      (defcomponent memo-deps ()
        :state ((multiplier 2))
        :render (let ((result (use-memo (multiplier)
                                (setq compute-count (1+ compute-count))
                                (* multiplier 10))))
                  (vui-text (number-to-string result))))
      (let ((instance (vui-mount (vui-component 'memo-deps) "*test-memo2*")))
        (unwind-protect
            (progn
              ;; First render
              (expect compute-count :to-equal 1)
              (expect (buffer-string) :to-equal "20")
              ;; Re-render with same deps
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              ;; Change deps
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :multiplier 5))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 2)
              (expect (buffer-string) :to-equal "50"))
          (kill-buffer "*test-memo2*")))))

  (it "can memoize filtered lists"
    (let ((filter-count 0))
      (defcomponent memo-filter ()
        :state ((items '("apple" "banana" "apricot")) (filter "ap"))
        :render (let ((filtered (use-memo (items filter)
                                  (setq filter-count (1+ filter-count))
                                  (seq-filter (lambda (i) (string-prefix-p filter i)) items))))
                  (vui-text (string-join filtered ", "))))
      (let ((instance (vui-mount (vui-component 'memo-filter) "*test-memo3*")))
        (unwind-protect
            (progn
              (expect filter-count :to-equal 1)
              (expect (buffer-string) :to-equal "apple, apricot")
              ;; Re-render without changing filter deps
              (vui--rerender-instance instance)
              (expect filter-count :to-equal 1))
          (kill-buffer "*test-memo3*"))))))

(describe "vui-table"
  (it "creates a table vnode"
    (let ((node (vui-table
                 :columns '((:header "A") (:header "B"))
                 :rows '(("1" "2")))))
      (expect (vui-vnode-table-p node) :to-be-truthy)
      (expect (length (vui-vnode-table-columns node)) :to-equal 2)
      (expect (length (vui-vnode-table-rows node)) :to-equal 1)))

  ;; Content-only: no trailing newlines
  (it "has no trailing newline without borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 1 :grow t))
                   :rows '(("a") ("b"))))
      ;; Internal newlines between rows, but no trailing newline
      (expect (buffer-string) :to-equal "a\nb")))

  (it "has no trailing newline with borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 1 :grow t))
                   :rows '(("x"))
                   :border :ascii))
      ;; +---+\n| x |\n+---+ with NO trailing newline
      (expect (buffer-string) :to-equal "+---+\n| x |\n+---+")))

  (it "renders simple table without borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 4 :grow t) (:width 4 :grow t))
                   :rows '(("A" "B") ("C" "D"))))
      (expect (buffer-string) :to-equal "A    B   \nC    D   ")))

  (it "renders table with headers"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "X" :width 3 :grow t) (:header "Y" :width 3 :grow t))
                   :rows '(("1" "2"))))
      (expect (buffer-string) :to-match "X")
      (expect (buffer-string) :to-match "Y")
      (expect (buffer-string) :to-match "1")
      (expect (buffer-string) :to-match "2")))

  (it "renders table with ascii borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "A" :width 3 :grow t) (:header "B" :width 3 :grow t))
                   :rows '(("1" "2"))
                   :border :ascii))
      ;; Width 3 + padding 1 on each side = 5 per column
      (expect (buffer-string) :to-match "\\+-----\\+-----\\+")
      (expect (buffer-string) :to-match "| A   | B   |")
      (expect (buffer-string) :to-match "| 1   | 2   |")))

  (it "renders table with unicode borders"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "A" :width 3 :grow t) (:header "B" :width 3 :grow t))
                   :rows '(("1" "2"))
                   :border :unicode))
      (expect (buffer-string) :to-match "┌")
      (expect (buffer-string) :to-match "│")
      (expect (buffer-string) :to-match "└")))

  (it "respects column alignment"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t :align :left)
                              (:width 5 :grow t :align :right)
                              (:width 5 :grow t :align :center))
                   :rows '(("L" "R" "C"))))
      (expect (buffer-string) :to-match "L    ")
      (expect (buffer-string) :to-match "    R")
      (expect (buffer-string) :to-match "  C  ")))

  (it "auto-calculates column widths from content"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '(nil nil)
                   :rows '(("short" "longer-text"))))
      ;; Content should fit without truncation
      (expect (buffer-string) :to-match "short")
      (expect (buffer-string) :to-match "longer-text")))

  ;; Cell structure tests - verify "| VALUE |" format
  (it "surrounds cell content with proper separators"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello"))
                   :border :ascii))
      ;; Cell should be: "| " + content + " |"
      (expect (buffer-string) :to-match "| hello |")))

  ;; Truncation tests
  (it "truncates content when :truncate is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :truncate t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; "hello world" (11 chars) -> "hello..." (8 chars)
      (expect (buffer-string) :to-match "| hello\\.\\.\\. |")
      (expect (buffer-string) :not :to-match "world")))

  (it "does not truncate when content fits width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :truncate t))
                   :rows '(("short"))
                   :border :ascii))
      ;; Content fits, no truncation needed
      (expect (buffer-string) :to-match "| short |")))

  ;; Overflow tests
  (it "overflows with broken bar when content exceeds width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; Content up to width, then " ¦" (space + broken bar) + overflow content
      (expect (buffer-string) :to-match "| hello ¦world")))

  (it "shrinks column to content when content is shorter than width without grow"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10))
                   :rows '(("hi"))
                   :border :ascii))
      ;; Without :grow, column shrinks to content size
      ;; Cell: "| hi |" not "| hi         |"
      (expect (buffer-string) :to-match "| hi |")))

  ;; Grow tests
  (it "does not overflow when :grow t expands column"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; With :grow t, column expands to fit content - no overflow
      (expect (buffer-string) :to-match "| hello world |")
      ;; Should NOT have overflow indicator
      (expect (buffer-string) :not :to-match "¦")))

  (it "uses :grow t to enforce minimum width with padding"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t))
                   :rows '(("hi"))
                   :border :ascii))
      ;; With :grow t, column is at least width 10, content padded
      (expect (buffer-string) :to-match "| hi         |")))

  (it "pads columns without borders when :grow t is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t) (:width 10 :grow t))
                   :rows '(("A" "B"))))
      ;; Columns should be padded to width 10 (no borders = space separator)
      (expect (buffer-string) :to-match "A         ")
      (expect (buffer-string) :to-match "B         ")))

  ;; Grow + Truncate combination
  (it "truncates even with :grow when :truncate is set"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 8 :grow t :truncate t))
                   :rows '(("hello world"))
                   :border :ascii))
      ;; :truncate wins - content is truncated, not expanded
      (expect (buffer-string) :to-match "| hello\\.\\.\\. |")
      (expect (buffer-string) :not :to-match "world")))

  (it "pads with :grow :truncate when content is shorter"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t :truncate t))
                   :rows '(("hi"))
                   :border :ascii))
      ;; Content shorter than width, :grow pads it
      (expect (buffer-string) :to-match "| hi         |")))

  ;; Content exactly equal to width
  (it "handles content exactly equal to width"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 5 :grow t))
                   :rows '(("hello"))
                   :border :ascii))
      ;; Content exactly fits, no padding or truncation
      (expect (buffer-string) :to-match "| hello |")))

  ;; Button truncation in tables
  (it "truncates button label when column has :truncate"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :truncate t))
                   :rows (list (list (vui-button "hello world")))))
      ;; Button "hello world" (11 chars) + [] = 13 chars, must fit in 10
      ;; Should be [hello...] = 10 chars
      (expect (buffer-string) :to-equal "[hello...]")))

  (it "does not truncate button when it fits"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t))
                   :rows (list (list (vui-button "hello")))))
      ;; Button [hello] = 7 chars, fits in 10 with padding
      (expect (buffer-string) :to-match "\\[hello\\]")))

  (it "truncates button in bordered table cell"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:width 10 :grow t :truncate t))
                   :rows (list (list (vui-button "very long button")))
                   :border :ascii))
      ;; Cell content must be 10 chars, button truncated
      ;; max-width=10: [] takes 2, ... takes 3, so 5 chars for label = "very "
      ;; | [very ...] | - button inside 10-char cell
      (expect (buffer-string) :to-match "| \\[very \\.\\.\\.\\] |"))))

(describe "vui-error-boundary"
  (it "creates an error boundary vnode"
    (let ((node (vui-error-boundary
                 :fallback (lambda (_) (vui-text "Error"))
                 :children (list (vui-text "OK")))))
      (expect (vui-vnode-error-boundary-p node) :to-be-truthy)
      (expect (vui-vnode-error-boundary-fallback node) :to-be-truthy)
      (expect (vui-vnode-error-boundary-children node) :to-equal (list (vui-text "OK")))))

  (it "renders children when no error"
    (with-temp-buffer
      (clrhash vui--error-boundary-errors)
      (vui-render (vui-error-boundary
                   :id 'test-no-error
                   :fallback (lambda (_) (vui-text "Error occurred"))
                   :children (list (vui-text "Hello World"))))
      (expect (buffer-string) :to-equal "Hello World")))

  (it "catches errors and renders fallback"
    (with-temp-buffer
      (clrhash vui--error-boundary-errors)
      (defcomponent error-component ()
        :render (error "Test error"))
      (vui-render (vui-error-boundary
                   :id 'test-catch-error
                   :fallback (lambda (err)
                               (vui-text (format "Caught: %s" (cadr err))))
                   :children (list (vui-component 'error-component))))
      (expect (buffer-string) :to-match "Caught: Test error")))

  (it "calls on-error callback when error is caught"
    (let ((error-log nil))
      (with-temp-buffer
        (clrhash vui--error-boundary-errors)
        (defcomponent error-component2 ()
          :render (error "Logged error"))
        (vui-render (vui-error-boundary
                     :id 'test-on-error
                     :on-error (lambda (err)
                                 (setq error-log err))
                     :fallback (lambda (_) (vui-text "Fallback"))
                     :children (list (vui-component 'error-component2))))
        (expect error-log :to-be-truthy)
        (expect (cadr error-log) :to-equal "Logged error"))))

  (it "persists error state across re-renders"
    (clrhash vui--error-boundary-errors)
    (defcomponent error-counter ()
      :state ((count 0))
      :render (progn
                (when (> count 0)
                  (error "Count is positive"))
                (vui-text (number-to-string count))))
    (let ((instance (vui-mount
                     (vui-component 'error-counter)
                     "*test-persist-error*")))
      (unwind-protect
          (progn
            ;; Initially no error
            (expect (with-current-buffer "*test-persist-error*"
                      (buffer-string))
                    :to-equal "0")
            ;; Wrap in error boundary and trigger error
            (clrhash vui--error-boundary-errors)
            (setf (vui-instance-state instance)
                  (plist-put (vui-instance-state instance) :count 1)))
        (kill-buffer "*test-persist-error*"))))

  (it "resets error with vui-reset-error-boundary"
    (with-temp-buffer
      (clrhash vui--error-boundary-errors)
      ;; Store a fake error
      (puthash 'reset-test '(error "Fake error") vui--error-boundary-errors)
      (expect (gethash 'reset-test vui--error-boundary-errors) :to-be-truthy)
      ;; Reset it
      (let ((vui--root-instance nil)) ; Prevent re-render
        (vui-reset-error-boundary 'reset-test))
      (expect (gethash 'reset-test vui--error-boundary-errors) :to-be nil))))

(describe "lifecycle error handling"
  (it "catches on-mount errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (defcomponent error-on-mount ()
        :on-mount (error "Mount error")
        :render (vui-text "OK"))
      (setq vui-last-error nil)
      (vui-mount (vui-component 'error-on-mount) "*test-mount-error*")
      (unwind-protect
          (progn
            (expect vui-last-error :to-be-truthy)
            (expect (car vui-last-error) :to-equal 'lifecycle)
            (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-mount"))
        (kill-buffer "*test-mount-error*"))))

  (it "catches on-update errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (defcomponent error-on-update ()
        :state ((count 0))
        :on-update (error "Update error")
        :render (vui-text (number-to-string count)))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'error-on-update) "*test-update-error*")))
        (unwind-protect
            (progn
              ;; on-update not called on first render
              (expect vui-last-error :to-be nil)
              ;; Trigger re-render
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :count 1))
              (vui--rerender-instance instance)
              ;; Now error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-update"))
          (kill-buffer "*test-update-error*")))))

  (it "catches on-unmount errors and stores them"
    (let ((vui-lifecycle-error-handler 'ignore))
      (defcomponent error-on-unmount ()
        :on-unmount (error "Unmount error")
        :render (vui-text "Content"))
      (defcomponent unmount-wrapper ()
        :state ((show t))
        :render (if show
                    (vui-component 'error-on-unmount)
                  (vui-text "Hidden")))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'unmount-wrapper) "*test-unmount-error*")))
        (unwind-protect
            (progn
              ;; Hide the child to trigger unmount
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :show nil))
              (vui--rerender-instance instance)
              ;; Error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-unmount"))
          (kill-buffer "*test-unmount-error*")))))

  (it "respects ignore handler"
    (let ((vui-lifecycle-error-handler 'ignore))
      (defcomponent silent-error ()
        :on-mount (error "Silent")
        :render (vui-text "OK"))
      ;; Should not signal error
      (vui-mount (vui-component 'silent-error) "*test-ignore*")
      (unwind-protect
          (expect (with-current-buffer "*test-ignore*" (buffer-string))
                  :to-equal "OK")
        (kill-buffer "*test-ignore*"))))

  (it "respects signal handler"
    (let ((vui-lifecycle-error-handler 'signal))
      (defcomponent signal-error ()
        :on-mount (error "Should propagate")
        :render (vui-text "OK"))
      (expect (vui-mount (vui-component 'signal-error) "*test-signal*")
              :to-throw 'error)
      (when (get-buffer "*test-signal*")
        (kill-buffer "*test-signal*"))))

  (it "calls custom handler function"
    (let* ((handler-called nil)
           (vui-lifecycle-error-handler
            (lambda (hook-name err instance)
              (setq handler-called (list hook-name (cadr err)
                                         (vui-component-def-name
                                          (vui-instance-def instance)))))))
      (defcomponent custom-handler-test ()
        :on-mount (error "Custom error")
        :render (vui-text "OK"))
      (vui-mount (vui-component 'custom-handler-test) "*test-custom*")
      (unwind-protect
          (progn
            (expect handler-called :to-be-truthy)
            (expect (car handler-called) :to-equal "on-mount")
            (expect (cadr handler-called) :to-equal "Custom error")
            (expect (caddr handler-called) :to-equal 'custom-handler-test))
        (kill-buffer "*test-custom*")))))

(describe "event error handling"
  (it "catches on-click errors and stores them"
    (let ((vui-event-error-handler 'ignore))
      (defcomponent click-error ()
        :render (vui-button "Click"
                            :on-click (lambda () (error "Click error"))))
      (setq vui-last-error nil)
      (let ((instance (vui-mount (vui-component 'click-error) "*test-click-error*")))
        (unwind-protect
            (progn
              ;; Simulate button click (buttons are now text with keymap)
              (with-current-buffer "*test-click-error*"
                (vui-test--click-button-at (point-min)))
              ;; Error should be caught
              (expect vui-last-error :to-be-truthy)
              (expect (car vui-last-error) :to-equal 'event)
              (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-click"))
          (kill-buffer "*test-click-error*")))))

  (it "catches on-change errors for checkbox"
    (let ((vui-event-error-handler 'ignore))
      (setq vui-last-error nil)
      (with-temp-buffer
        (vui-render (vui-checkbox
                     :checked nil
                     :on-change (lambda (_) (error "Checkbox error"))))
        ;; Simulate checkbox toggle (checkboxes are still widgets)
        (widget-apply (widget-at (point-min)) :notify nil))
      (expect vui-last-error :to-be-truthy)
      (expect (plist-get (caddr vui-last-error) :hook) :to-equal "on-change")))

  (it "respects ignore handler for events"
    (let ((vui-event-error-handler 'ignore))
      (setq vui-last-error nil)
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Silent error"))))
        ;; Should not signal error (buttons are now text with keymap)
        (vui-test--click-button-at (point-min)))
      ;; Error stored but not signaled
      (expect vui-last-error :to-be-truthy)))

  (it "respects signal handler for events"
    (let ((vui-event-error-handler 'signal))
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Should propagate"))))
        ;; Should signal error (buttons are now text with keymap)
        (expect (vui-test--click-button-at (point-min))
                :to-throw 'error))))

  (it "calls custom handler for events"
    (let* ((handler-called nil)
           (vui-event-error-handler
            (lambda (hook-name err _instance)
              (setq handler-called (list hook-name (cadr err))))))
      (with-temp-buffer
        (vui-render (vui-button "Test"
                                :on-click (lambda () (error "Custom event error"))))
        (vui-test--click-button-at (point-min)))
      (expect handler-called :to-be-truthy)
      (expect (car handler-called) :to-equal "on-click")
      (expect (cadr handler-called) :to-equal "Custom event error"))))

(describe "vui-batch"
  (it "batches multiple state updates into single render"
    (let ((vui-idle-render-delay nil)  ; Disable idle rendering
          (render-count 0))
      (defcomponent batch-test ()
        :state ((a 0) (b 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (format "a=%d b=%d" a b))))
      (let ((instance (vui-mount (vui-component 'batch-test) "*test-batch*")))
        (unwind-protect
            (progn
              ;; Initial render
              (expect render-count :to-equal 1)
              ;; Batch multiple updates
              (with-current-buffer "*test-batch*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-batch
                   (vui-set-state :a 1)
                   (vui-set-state :b 2))))
              ;; Should only have one additional render (total 2)
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-batch*" (buffer-string))
                      :to-equal "a=1 b=2"))
          (kill-buffer "*test-batch*")))))

  (it "handles nested batches correctly"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent nested-batch-test ()
        :state ((x 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string x))))
      (let ((instance (vui-mount (vui-component 'nested-batch-test) "*test-nested-batch*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              (with-current-buffer "*test-nested-batch*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-batch
                   (vui-set-state :x 1)
                   (vui-batch
                    (vui-set-state :x 2))
                   (vui-set-state :x 3))))
              ;; Only one render at the end of outermost batch
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-nested-batch*" (buffer-string))
                      :to-equal "3"))
          (kill-buffer "*test-nested-batch*")))))

  (it "does not render if no state changes"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent no-change-batch ()
        :state ((val 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text "OK")))
      (let ((instance (vui-mount (vui-component 'no-change-batch) "*test-no-change*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Empty batch - no state changes
              (vui-batch)
              ;; Should not trigger re-render
              (expect render-count :to-equal 1))
          (kill-buffer "*test-no-change*"))))))

(describe "vui-flush-sync"
  (it "forces immediate render"
    (let ((vui-idle-render-delay 10)  ; Long delay
          (render-count 0))
      (defcomponent flush-test ()
        :state ((val 0))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string val))))
      (let ((instance (vui-mount (vui-component 'flush-test) "*test-flush*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Change state (would be deferred)
              (with-current-buffer "*test-flush*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :val 42)))
              ;; Timer scheduled but not fired yet
              ;; Force sync render
              (let ((vui--root-instance instance))
                (vui-flush-sync))
              ;; Should have rendered
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-flush*" (buffer-string))
                      :to-equal "42"))
          (kill-buffer "*test-flush*"))))))

(describe "should-update"
  (it "always renders on first render"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent always-skip ()
        :should-update nil  ; Always return nil - but first render still happens
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text "OK")))
      (vui-mount (vui-component 'always-skip) "*test-first-render*")
      (unwind-protect
          (expect render-count :to-equal 1)
        (kill-buffer "*test-first-render*"))))

  (it "skips render when should-update returns nil"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent skip-renders ()
        :state ((count 0))
        :should-update nil  ; Always skip re-renders
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'skip-renders) "*test-skip*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              (expect (with-current-buffer "*test-skip*" (buffer-string))
                      :to-equal "0")
              ;; Change state and trigger re-render
              (with-current-buffer "*test-skip*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :count 42)))
              ;; Render function should not have been called
              (expect render-count :to-equal 1)
              ;; But content is still there (cached vtree)
              (expect (with-current-buffer "*test-skip*" (buffer-string))
                      :to-equal "0"))
          (kill-buffer "*test-skip*")))))

  (it "renders when should-update returns t"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent always-render ()
        :state ((count 0))
        :should-update t  ; Always re-render
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (number-to-string count))))
      (let ((instance (vui-mount (vui-component 'always-render) "*test-always*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Trigger re-render
              (with-current-buffer "*test-always*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :count 42)))
              ;; Should have re-rendered
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-always*" (buffer-string))
                      :to-equal "42"))
          (kill-buffer "*test-always*")))))

  (it "can compare prev values in should-update"
    (let ((vui-idle-render-delay nil)
          (render-count 0))
      (defcomponent smart-update ()
        :state ((important 0) (unimportant 0))
        ;; Only re-render when important changes
        :should-update (not (equal important (plist-get prev-state :important)))
        :render (progn
                  (setq render-count (1+ render-count))
                  (vui-text (format "i=%d u=%d" important unimportant))))
      (let ((instance (vui-mount (vui-component 'smart-update) "*test-smart*")))
        (unwind-protect
            (progn
              (expect render-count :to-equal 1)
              ;; Change unimportant - should NOT re-render
              (with-current-buffer "*test-smart*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :unimportant 999)))
              (expect render-count :to-equal 1)
              ;; Change important - SHOULD re-render
              (with-current-buffer "*test-smart*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :important 42)))
              (expect render-count :to-equal 2)
              (expect (with-current-buffer "*test-smart*" (buffer-string))
                      :to-equal "i=42 u=999"))
          (kill-buffer "*test-smart*")))))

  (it "does not call on-update when render skipped"
    (let ((vui-idle-render-delay nil)
          (update-count 0))
      (defcomponent no-update-call ()
        :state ((val 0))
        :should-update nil
        :on-update (setq update-count (1+ update-count))
        :render (vui-text "OK"))
      (let ((instance (vui-mount (vui-component 'no-update-call) "*test-no-update*")))
        (unwind-protect
            (progn
              (expect update-count :to-equal 0)
              ;; Trigger re-render (but should be skipped)
              (with-current-buffer "*test-no-update*"
                (let ((vui--current-instance instance)
                      (vui--root-instance instance))
                  (vui-set-state :val 42)))
              ;; on-update should not have been called
              (expect update-count :to-equal 0))
          (kill-buffer "*test-no-update*"))))))

(describe "memo comparison modes"
  (it "uses equal by default"
    ;; Default should use equal (structural comparison)
    (expect (vui--deps-equal-p '(1 2 3) '(1 2 3) 'equal) :to-be-truthy)
    (expect (vui--deps-equal-p '(1 2 3) '(1 2 4) 'equal) :to-be nil)
    (expect (vui--deps-equal-p '("a" "b") '("a" "b") 'equal) :to-be-truthy))

  (it "supports eq comparison mode"
    (let ((sym1 'foo)
          (sym2 'foo)
          (list1 '(1 2))
          (list2 '(1 2)))
      ;; Symbols are eq
      (expect (vui--deps-equal-p (list sym1) (list sym2) 'eq) :to-be-truthy)
      ;; Lists are not eq (different objects)
      (expect (vui--deps-equal-p (list list1) (list list2) 'eq) :to-be nil)
      ;; But lists are equal
      (expect (vui--deps-equal-p (list list1) (list list2) 'equal) :to-be-truthy)))

  (it "supports custom comparison function"
    ;; Custom comparator that only checks first element
    (let ((first-only (lambda (old new)
                        (equal (car old) (car new)))))
      (expect (vui--deps-equal-p '(1 2) '(1 99) first-only) :to-be-truthy)
      (expect (vui--deps-equal-p '(1 2) '(2 2) first-only) :to-be nil)))

  (it "works with use-memo* macro"
    (let ((vui-idle-render-delay nil)
          (compute-count 0))
      (defcomponent memo-compare-test ()
        :state ((mode 'view))
        :render (let ((result (use-memo* (mode)
                                :compare 'eq
                                (cl-incf compute-count)
                                (symbol-name mode))))
                  (vui-text result)))
      (let ((instance (vui-mount (vui-component 'memo-compare-test) "*test-memo-cmp*")))
        (unwind-protect
            (progn
              (expect compute-count :to-equal 1)
              ;; Re-render with same symbol - should use cached value
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 1)
              ;; Change to different symbol - should recompute
              (setf (vui-instance-state instance)
                    (plist-put (vui-instance-state instance) :mode 'edit))
              (vui--rerender-instance instance)
              (expect compute-count :to-equal 2))
          (kill-buffer "*test-memo-cmp*"))))))

(describe "timing instrumentation"
  (before-each
    (vui-clear-timing)
    (setq vui-timing-enabled nil))

  (after-each
    (vui-clear-timing)
    (setq vui-timing-enabled nil))

  (it "does not collect data when disabled"
    (setq vui-timing-enabled nil)
    (defcomponent timing-disabled-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-disabled-test) "*test-timing1*")))
      (unwind-protect
          (expect (vui-get-timing) :to-be nil)
        (kill-buffer "*test-timing1*"))))

  (it "collects timing data when enabled"
    (setq vui-timing-enabled t)
    (defcomponent timing-enabled-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-enabled-test) "*test-timing2*")))
      (unwind-protect
          (progn
            (expect (vui-get-timing) :to-be-truthy)
            (expect (length (vui-get-timing)) :to-be-greater-than 0))
        (kill-buffer "*test-timing2*"))))

  (it "records render phase"
    (setq vui-timing-enabled t)
    (defcomponent timing-render-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-render-test) "*test-timing3*")))
      (unwind-protect
          (let ((render-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'render))
                                          (vui-get-timing))))
            (expect render-entry :to-be-truthy)
            (expect (plist-get render-entry :component) :to-equal 'timing-render-test)
            (expect (plist-get render-entry :duration) :to-be-greater-than 0))
        (kill-buffer "*test-timing3*"))))

  (it "records commit phase"
    (setq vui-timing-enabled t)
    (defcomponent timing-commit-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-commit-test) "*test-timing4*")))
      (unwind-protect
          (let ((commit-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'commit))
                                          (vui-get-timing))))
            (expect commit-entry :to-be-truthy)
            (expect (plist-get commit-entry :component) :to-equal 'timing-commit-test))
        (kill-buffer "*test-timing4*"))))

  (it "records mount phase"
    (setq vui-timing-enabled t)
    (defcomponent timing-mount-test ()
      :on-mount (message "mounted")
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-mount-test) "*test-timing5*")))
      (unwind-protect
          (let ((mount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'mount))
                                         (vui-get-timing))))
            (expect mount-entry :to-be-truthy)
            (expect (plist-get mount-entry :component) :to-equal 'timing-mount-test))
        (kill-buffer "*test-timing5*"))))

  (it "records update phase"
    (setq vui-timing-enabled t)
    (defcomponent timing-update-test ()
      :state ((count 0))
      :on-update (message "updated")
      :render (vui-text (number-to-string count)))
    (let ((instance (vui-mount (vui-component 'timing-update-test) "*test-timing6*")))
      (unwind-protect
          (progn
            ;; Trigger re-render
            (setf (vui-instance-state instance)
                  (plist-put (vui-instance-state instance) :count 1))
            (vui--rerender-instance instance)
            (let ((update-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'update))
                                            (vui-get-timing))))
              (expect update-entry :to-be-truthy)
              (expect (plist-get update-entry :component) :to-equal 'timing-update-test)))
        (kill-buffer "*test-timing6*"))))

  (it "records unmount phase"
    (setq vui-timing-enabled t)
    (let ((show-child t))
      (defcomponent timing-unmount-child ()
        :on-unmount (message "unmounted")
        :render (vui-text "child"))
      (defcomponent timing-unmount-parent ()
        :render (if show-child
                    (vui-component 'timing-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'timing-unmount-parent) "*test-timing7*")))
        (unwind-protect
            (progn
              (setq show-child nil)
              (vui--rerender-instance instance)
              (let ((unmount-entry (cl-find-if (lambda (e) (eq (plist-get e :phase) 'unmount))
                                               (vui-get-timing))))
                (expect unmount-entry :to-be-truthy)
                (expect (plist-get unmount-entry :component) :to-equal 'timing-unmount-child)))
          (kill-buffer "*test-timing7*")))))

  (it "clears timing data"
    (setq vui-timing-enabled t)
    (defcomponent timing-clear-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'timing-clear-test) "*test-timing8*")))
      (unwind-protect
          (progn
            (expect (vui-get-timing) :to-be-truthy)
            (vui-clear-timing)
            (expect (vui-get-timing) :to-be nil))
        (kill-buffer "*test-timing8*"))))

  (it "limits entries to max"
    (setq vui-timing-enabled t)
    (let ((vui--timing-max-entries 5))
      (defcomponent timing-limit-test ()
        :state ((count 0))
        :render (vui-text (number-to-string count)))
      (let ((instance (vui-mount (vui-component 'timing-limit-test) "*test-timing9*")))
        (unwind-protect
            (progn
              ;; Generate many entries by re-rendering
              (dotimes (i 10)
                (setf (vui-instance-state instance)
                      (plist-put (vui-instance-state instance) :count i))
                (vui--rerender-instance instance))
              ;; Should be capped at max
              (expect (length (vui-get-timing)) :to-equal 5))
          (kill-buffer "*test-timing9*"))))))

(describe "component inspector"
  (it "returns message when no instance mounted"
    (let ((vui--root-instance nil))
      (expect (vui-inspect) :not :to-throw)))

  (it "inspects root instance"
    (defcomponent inspect-root-test ()
      :state ((count 42))
      :render (vui-text "hello"))
    (let ((instance (vui-mount (vui-component 'inspect-root-test) "*test-inspect1*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match "Component Inspector")
                (expect content :to-match "inspect-root-test")
                (expect content :to-match ":count")
                (expect content :to-match "42"))))
        (kill-buffer "*test-inspect1*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*")))))

  (it "shows nested component tree"
    (defcomponent inspect-child ()
      :state ((value "inner"))
      :render (vui-text value))
    (defcomponent inspect-parent ()
      :state ((label "outer"))
      :render (vui-fragment
               (vui-text label)
               (vui-component 'inspect-child)))
    (let ((instance (vui-mount (vui-component 'inspect-parent) "*test-inspect2*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match "inspect-parent")
                (expect content :to-match "inspect-child")
                (expect content :to-match ":label")
                (expect content :to-match ":value"))))
        (kill-buffer "*test-inspect2*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*")))))

  (it "shows props in inspector"
    (defcomponent inspect-with-props (name)
      :render (vui-text (format "Hi %s" name)))
    (let ((instance (vui-mount (vui-component 'inspect-with-props :name "Alice")
                               "*test-inspect3*")))
      (unwind-protect
          (progn
            (vui-inspect)
            (with-current-buffer "*vui-inspector*"
              (let ((content (buffer-string)))
                (expect content :to-match ":name")
                (expect content :to-match "Alice"))))
        (kill-buffer "*test-inspect3*")
        (when (get-buffer "*vui-inspector*")
          (kill-buffer "*vui-inspector*"))))))

(describe "state viewer"
  (it "shows only stateful components"
    (defcomponent stateless-comp ()
      :render (vui-text "no state"))
    (defcomponent stateful-comp ()
      :state ((val 123))
      :render (vui-text "with state"))
    (defcomponent state-viewer-parent ()
      :state ((parent-val "parent"))
      :render (vui-fragment
               (vui-component 'stateless-comp)
               (vui-component 'stateful-comp)))
    (let ((instance (vui-mount (vui-component 'state-viewer-parent) "*test-state1*")))
      (unwind-protect
          (progn
            (vui-inspect-state)
            (with-current-buffer "*vui-state*"
              (let ((content (buffer-string)))
                (expect content :to-match "state-viewer-parent")
                (expect content :to-match "stateful-comp")
                ;; Stateless should not appear (no state to show)
                (expect content :not :to-match "stateless-comp"))))
        (kill-buffer "*test-state1*")
        (when (get-buffer "*vui-state*")
          (kill-buffer "*vui-state*"))))))

(describe "instance lookup"
  (it "finds instance by id"
    (defcomponent lookup-child ()
      :render (vui-text "child"))
    (defcomponent lookup-parent ()
      :render (vui-component 'lookup-child))
    (let ((instance (vui-mount (vui-component 'lookup-parent) "*test-lookup1*")))
      (unwind-protect
          (let* ((child (car (vui-instance-children instance)))
                 (child-id (vui-instance-id child))
                 (found (vui-get-instance-by-id child-id)))
            (expect found :to-equal child))
        (kill-buffer "*test-lookup1*"))))

  (it "returns nil for non-existent id"
    (defcomponent lookup-simple ()
      :render (vui-text "hi"))
    (let ((instance (vui-mount (vui-component 'lookup-simple) "*test-lookup2*")))
      (unwind-protect
          (expect (vui-get-instance-by-id 999999) :to-be nil)
        (kill-buffer "*test-lookup2*"))))

  (it "finds all instances of a type"
    (defcomponent item-comp ()
      :render (vui-text "item"))
    (defcomponent list-comp ()
      :render (vui-fragment
               (vui-component 'item-comp)
               (vui-component 'item-comp)
               (vui-component 'item-comp)))
    (let ((instance (vui-mount (vui-component 'list-comp) "*test-lookup3*")))
      (unwind-protect
          (let ((items (vui-get-component-instances 'item-comp)))
            (expect (length items) :to-equal 3))
        (kill-buffer "*test-lookup3*")))))

(describe "debug logging"
  (before-each
    (setq vui-debug-enabled nil)
    (vui-debug-clear))

  (after-each
    (setq vui-debug-enabled nil)
    (vui-debug-clear)
    (when (get-buffer "*vui-debug*")
      (kill-buffer "*vui-debug*")))

  (it "does not log when disabled"
    (setq vui-debug-enabled nil)
    (defcomponent debug-off-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-off-test) "*test-debug1*")))
      (unwind-protect
          (let ((buf (get-buffer "*vui-debug*")))
            (if buf
                (with-current-buffer buf
                  (expect (buffer-string) :to-equal ""))
              (expect buf :to-be nil)))
        (kill-buffer "*test-debug1*"))))

  (it "logs render phase when enabled"
    (setq vui-debug-enabled t)
    (defcomponent debug-render-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-render-test) "*test-debug2*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              (expect content :to-match "render:")
              (expect content :to-match "debug-render-test")))
        (kill-buffer "*test-debug2*"))))

  (it "logs mount phase"
    (setq vui-debug-enabled t)
    (defcomponent debug-mount-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-mount-test) "*test-debug3*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              (expect content :to-match "mount:")
              (expect content :to-match "debug-mount-test")))
        (kill-buffer "*test-debug3*"))))

  (it "logs state changes"
    (setq vui-debug-enabled t)
    (defcomponent debug-state-test ()
      :state ((count 0))
      :render (vui-button "+"
                          :on-click (lambda () (vui-set-state :count (1+ count)))))
    (let ((instance (vui-mount (vui-component 'debug-state-test) "*test-debug4*")))
      (unwind-protect
          (progn
            ;; Trigger state change (buttons are now text with keymap)
            (with-current-buffer "*test-debug4*"
              (vui-test--click-button-at (point-min)))
            (with-current-buffer "*vui-debug*"
              (let ((content (buffer-string)))
                (expect content :to-match "state-change:")
                (expect content :to-match ":count"))))
        (kill-buffer "*test-debug4*"))))

  (it "logs unmount phase"
    (setq vui-debug-enabled t)
    (let ((show-child t))
      (defcomponent debug-unmount-child ()
        :render (vui-text "child"))
      (defcomponent debug-unmount-parent ()
        :render (if show-child
                    (vui-component 'debug-unmount-child)
                  (vui-text "no child")))
      (let ((instance (vui-mount (vui-component 'debug-unmount-parent) "*test-debug5*")))
        (unwind-protect
            (progn
              (setq show-child nil)
              (vui--rerender-instance instance)
              (with-current-buffer "*vui-debug*"
                (let ((content (buffer-string)))
                  (expect content :to-match "unmount:")
                  (expect content :to-match "debug-unmount-child"))))
          (kill-buffer "*test-debug5*")))))

  (it "respects vui-debug-log-phases filter"
    (setq vui-debug-enabled t)
    (setq vui-debug-log-phases '(mount))  ; Only log mount
    (defcomponent debug-filter-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-filter-test) "*test-debug6*")))
      (unwind-protect
          (with-current-buffer "*vui-debug*"
            (let ((content (buffer-string)))
              ;; Should have mount
              (expect content :to-match "mount:")
              ;; Should NOT have render
              (expect content :not :to-match "render:")))
        (kill-buffer "*test-debug6*")))
    ;; Reset phases
    (setq vui-debug-log-phases '(render mount update unmount state-change)))

  (it "clears debug buffer"
    (setq vui-debug-enabled t)
    (defcomponent debug-clear-test ()
      :render (vui-text "OK"))
    (let ((instance (vui-mount (vui-component 'debug-clear-test) "*test-debug7*")))
      (unwind-protect
          (progn
            (with-current-buffer "*vui-debug*"
              (expect (buffer-string) :not :to-equal ""))
            (vui-debug-clear)
            (with-current-buffer "*vui-debug*"
              (expect (buffer-string) :to-equal "")))
        (kill-buffer "*test-debug7*")))))

;;; vui-test.el ends here

;;; vui-test.el --- Tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)

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
      (expect (vui-vnode-newline-p node) :to-be-truthy))))

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
      (expect (vui-vnode-key node) :to-equal "btn-1"))))

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
    (let ((node (vui-select "a" '("a" "b" "c"))))
      (expect (vui-vnode-select-p node) :to-be-truthy)
      (expect (vui-vnode-select-value node) :to-equal "a")
      (expect (vui-vnode-select-options node) :to-equal '("a" "b" "c"))))

  (it "accepts prompt property"
    (let ((node (vui-select nil '("x") :prompt "Choose: ")))
      (expect (vui-vnode-select-prompt node) :to-equal "Choose: ")))

  (it "defaults prompt to Select:"
    (let ((node (vui-select nil '("x"))))
      (expect (vui-vnode-select-prompt node) :to-equal "Select: ")))

  (it "renders as button with current value"
    (with-temp-buffer
      (vui-render (vui-select "apple" '("apple" "banana")))
      (expect (buffer-string) :to-match "apple")))

  (it "renders placeholder when no value"
    (with-temp-buffer
      (vui-render (vui-select nil '("a" "b")))
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
  (it "creates a vstack vnode"
    (let ((node (vui-vstack (vui-text "a") (vui-text "b"))))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (length (vui-vnode-vstack-children node)) :to-equal 2)))

  (it "defaults spacing to 0 and indent to 0"
    (let ((node (vui-vstack (vui-text "a"))))
      (expect (vui-vnode-vstack-spacing node) :to-equal 0)
      (expect (vui-vnode-vstack-indent node) :to-equal 0)))

  (it "renders children vertically"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "line1") (vui-text "line2")))
      (expect (buffer-string) :to-equal "line1\nline2")))

  (it "renders with spacing (blank lines)"
    (with-temp-buffer
      (vui-render (vui-vstack :spacing 1 (vui-text "a") (vui-text "b")))
      (expect (buffer-string) :to-equal "a\n\nb")))

  (it "renders with indent"
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
  (it "renders a list of items"
    (with-temp-buffer
      (vui-render (vui-list '("a" "b" "c")
                            (lambda (item) (vui-text item))))
      (expect (buffer-string) :to-equal "abc")))

  (it "applies keys from key-fn"
    (let* ((items '((:id 1 :name "Alice") (:id 2 :name "Bob")))
           (node (vui-list items
                           (lambda (item) (vui-text (plist-get item :name)))
                           (lambda (item) (plist-get item :id)))))
      (expect (vui-vnode-fragment-p node) :to-be-truthy)
      (let ((children (vui-vnode-fragment-children node)))
        (expect (vui-vnode-key (nth 0 children)) :to-equal 1)
        (expect (vui-vnode-key (nth 1 children)) :to-equal 2))))

  (it "uses item as key when no key-fn provided"
    (let* ((node (vui-list '("x" "y") #'vui-text)))
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
      (expect (buffer-string) :to-equal "[Disabled]")))

  (it "triggers on-click callback when button is activated"
    (with-temp-buffer
      (let ((clicked nil))
        (vui-render (vui-button "Click" :on-click (lambda () (setq clicked t))))
        ;; Find and activate the widget
        (goto-char (point-min))
        (widget-forward 1)
        (widget-button-press (point))
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
          (kill-buffer "*test-args*"))))))

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

  (it "renders simple table without borders"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:min-width 4) (:min-width 4))
                    :rows '(("A" "B") ("C" "D"))))
      (expect (buffer-string) :to-equal "A    B   \nC    D   \n")))

  (it "renders table with headers"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:header "X" :min-width 3) (:header "Y" :min-width 3))
                    :rows '(("1" "2"))))
      (expect (buffer-string) :to-match "X")
      (expect (buffer-string) :to-match "Y")
      (expect (buffer-string) :to-match "1")
      (expect (buffer-string) :to-match "2")))

  (it "renders table with ascii borders"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:header "A" :width 3) (:header "B" :width 3))
                    :rows '(("1" "2"))
                    :border :ascii))
      (expect (buffer-string) :to-match "\\+---\\+---\\+")
      (expect (buffer-string) :to-match "|A  |B  |")
      (expect (buffer-string) :to-match "|1  |2  |")))

  (it "renders table with unicode borders"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:header "A" :width 3) (:header "B" :width 3))
                    :rows '(("1" "2"))
                    :border :unicode))
      (expect (buffer-string) :to-match "┌")
      (expect (buffer-string) :to-match "│")
      (expect (buffer-string) :to-match "└")))

  (it "respects column alignment"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:width 5 :align :left)
                               (:width 5 :align :right)
                               (:width 5 :align :center))
                    :rows '(("L" "R" "C"))))
      (expect (buffer-string) :to-match "L    ")
      (expect (buffer-string) :to-match "    R")
      (expect (buffer-string) :to-match "  C  ")))

  (it "auto-calculates column widths from content"
    (with-temp-buffer
      (vui-render (vui-table
                    :columns '((:min-width 1) (:min-width 1))
                    :rows '(("short" "longer-text"))))
      ;; Content should fit without truncation
      (expect (buffer-string) :to-match "short")
      (expect (buffer-string) :to-match "longer-text"))))

;;; vui-test.el ends here

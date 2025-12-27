;;; vui-component-test.el --- Component tests for vui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vui.el component system: defcomponent, rendering, lifecycle, reconciliation.

;;; Code:

(require 'buttercup)
(require 'vui)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-test--click-button-at (pos)
  "Invoke the button widget at POS."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(describe "defcomponent"
  (it "defines a component and registers it"
    (vui-defcomponent test-simple ()
      :render (vui-text "simple"))
    (expect (vui--get-component 'test-simple) :to-be-truthy))

  (it "defines component with props"
    (vui-defcomponent test-greeting (name)
      :render (vui-text (format "Hello, %s!" name)))
    (let ((def (vui--get-component 'test-greeting)))
      (expect (vui-component-def-props-spec def) :to-equal '(name))))

  (it "defines component with state"
    (vui-defcomponent test-counter ()
      :state ((count 0))
      :render (vui-text (number-to-string count)))
    (let ((def (vui--get-component 'test-counter)))
      (expect (vui-component-def-initial-state-fn def) :to-be-truthy)))

  (it "allows state initializers to access props"
    (vui-defcomponent test-prop-to-state (initial-value)
      :state ((value initial-value))  ; state initialized from prop
      :render (vui-text (format "Value: %s" value)))
    (with-temp-buffer
      (vui-render (vui-component 'test-prop-to-state :initial-value "hello"))
      (expect (buffer-string) :to-equal "Value: hello")))

  (it "supports optional docstring"
    (vui-defcomponent test-with-docstring (name)
      "A component with documentation."
      :render (vui-text name))
    (let ((def (vui--get-component 'test-with-docstring)))
      (expect (vui-component-def-docstring def)
              :to-equal "A component with documentation.")))

  (it "works without docstring"
    (vui-defcomponent test-no-docstring (name)
      :render (vui-text name))
    (let ((def (vui--get-component 'test-no-docstring)))
      (expect (vui-component-def-docstring def) :to-be nil))))

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
    (vui-defcomponent render-test ()
      :render (vui-text "rendered!"))
    (with-temp-buffer
      (vui-render (vui-component 'render-test))
      (expect (buffer-string) :to-equal "rendered!")))

  (it "renders component with props"
    (vui-defcomponent greeting-test (name)
      :render (vui-text (format "Hi, %s!" name)))
    (with-temp-buffer
      (vui-render (vui-component 'greeting-test :name "Alice"))
      (expect (buffer-string) :to-equal "Hi, Alice!")))

  (it "renders nested components"
    (vui-defcomponent inner-comp ()
      :render (vui-text "[inner]"))
    (vui-defcomponent outer-comp ()
      :render (vui-fragment
               (vui-text "outer:")
               (vui-component 'inner-comp)))
    (with-temp-buffer
      (vui-render (vui-component 'outer-comp))
      (expect (buffer-string) :to-equal "outer:[inner]")))

  (it "passes children to component"
    (vui-defcomponent wrapper-comp ()
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
      (vui-defcomponent mount-test ()
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
      (vui-defcomponent mount-once ()
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

  (it "triggers re-render when vui-set-state called in on-mount"
    (let ((render-count 0))
      (vui-defcomponent mount-set-state ()
        :state ((value "initial"))
        :on-mount
        (vui-set-state :value "from-mount")
        :render
        (progn
          (setq render-count (1+ render-count))
          (vui-text value)))
      (let ((instance (vui-mount (vui-component 'mount-set-state) "*test-mount-state*")))
        (unwind-protect
            (progn
              ;; Flush the deferred render scheduled by vui-set-state
              (vui-flush-sync)
              ;; Should have rendered twice: initial + after on-mount set state
              (expect render-count :to-equal 2)
              ;; Final state should be from on-mount
              (expect (buffer-string) :to-equal "from-mount"))
          (kill-buffer "*test-mount-state*")))))

  (it "triggers re-render when vui-batch used in on-mount"
    (let ((render-count 0))
      (vui-defcomponent mount-batch-state ()
        :state ((a "initial-a")
                (b "initial-b"))
        :on-mount
        (vui-batch
         (vui-set-state :a "from-mount-a")
         (vui-set-state :b "from-mount-b"))
        :render
        (progn
          (setq render-count (1+ render-count))
          (vui-text (concat a " " b))))
      (let ((instance (vui-mount (vui-component 'mount-batch-state) "*test-mount-batch*")))
        (unwind-protect
            (progn
              ;; Flush the deferred render scheduled by vui-batch
              (vui-flush-sync)
              ;; Should have rendered twice: initial + after on-mount batch
              (expect render-count :to-equal 2)
              ;; Final state should be from on-mount
              (expect (buffer-string) :to-equal "from-mount-a from-mount-b"))
          (kill-buffer "*test-mount-batch*")))))

  (it "deferred timer fires automatically for on-mount state changes"
    (let ((render-count 0))
      (vui-defcomponent mount-timer-test ()
        :state ((value "initial"))
        :on-mount
        (vui-set-state :value "from-mount")
        :render
        (progn
          (setq render-count (1+ render-count))
          (vui-text value)))
      (let ((instance (vui-mount (vui-component 'mount-timer-test) "*test-timer*")))
        (unwind-protect
            (progn
              ;; First render should have happened
              (expect render-count :to-equal 1)
              (expect (buffer-string) :to-equal "initial")
              ;; Let deferred timer fire (vui-render-delay is 0.01s)
              (sleep-for 0.05)
              ;; Second render should have happened
              (expect render-count :to-equal 2)
              (expect (buffer-string) :to-equal "from-mount"))
          (kill-buffer "*test-timer*")))))

  (it "vui-batch works inside with-temp-buffer in on-mount"
    (let ((render-count 0))
      (vui-defcomponent mount-temp-buffer-test ()
        :state ((data nil))
        :on-mount
        ;; Simulate loading data from a file using with-temp-buffer
        (with-temp-buffer
          (insert "loaded-data")
          (let ((loaded (buffer-string)))
            (vui-batch
             (vui-set-state :data loaded))))
        :render
        (progn
          (setq render-count (1+ render-count))
          (vui-text (or data "none"))))
      (let ((instance (vui-mount (vui-component 'mount-temp-buffer-test) "*test-temp-buf*")))
        (unwind-protect
            (progn
              ;; Let deferred timer fire
              (sleep-for 0.05)
              ;; Should have rendered twice and show loaded data
              (expect render-count :to-equal 2)
              (expect (buffer-string) :to-equal "loaded-data"))
          (kill-buffer "*test-temp-buf*")))))

  (it "calls on-unmount when component is removed"
    (let ((unmounted nil)
          (show-child t))
      (vui-defcomponent unmount-child ()
        :on-unmount (setq unmounted t)
        :render (vui-text "child"))
      (vui-defcomponent unmount-parent ()
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
      (vui-defcomponent nested-inner ()
        :on-unmount (push 'inner unmount-order)
        :render (vui-text "inner"))
      (vui-defcomponent nested-outer ()
        :on-unmount (push 'outer unmount-order)
        :render (vui-fragment
                 (vui-text "outer:")
                 (vui-component 'nested-inner)))
      (vui-defcomponent nested-root ()
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
      (vui-defcomponent lifecycle-args (name)
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
      (vui-defcomponent test-update ()
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
      (vui-defcomponent test-update-props (label)
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
    (vui-defcomponent stateful-child ()
      :state ((value 42))
      :render (vui-text (number-to-string value)))
    ;; Define parent that renders the child
    (vui-defcomponent parent-comp ()
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
    (vui-defcomponent indexed-child (label)
      :state ((count 0))
      :render (vui-text (format "%s:%d" label count)))
    (vui-defcomponent indexed-parent ()
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
        (kill-buffer "*test-idx*"))))

  (it "preserves state when items reordered by key"
    ;; Define a child component with internal state
    (vui-defcomponent keyed-item (id)
      :state ((clicks 0))
      :render (vui-button (format "%s:%d" id clicks)
                :on-click (lambda () (vui-set-state :clicks (1+ clicks)))))
    ;; Parent component that renders a list with keys
    (let ((items '("A" "B" "C")))
      (vui-defcomponent keyed-list ()
        :render (apply #'vui-vstack
                  (mapcar (lambda (id)
                            (vui-component 'keyed-item :key id :id id))
                          items)))
      (let ((instance (vui-mount (vui-component 'keyed-list) "*test-keys*")))
        (unwind-protect
            (with-current-buffer "*test-keys*"
              ;; Click first button (A) at point-min to increment its state
              (vui-test--click-button-at (point-min))
              ;; Force immediate render (vui-render-delay causes async render)
              (vui-flush-sync)
              ;; Verify A now shows clicks=1
              (expect (buffer-string) :to-match "A:1")
              ;; Reorder: move A to last position
              (setq items '("B" "C" "A"))
              (vui--rerender-instance instance)
              ;; A should still have clicks=1 despite position change
              (expect (buffer-string) :to-match "A:1")
              ;; B and C should still have clicks=0
              (expect (buffer-string) :to-match "B:0")
              (expect (buffer-string) :to-match "C:0"))
          (kill-buffer "*test-keys*"))))))

(describe "cursor preservation"
  (it "maintains cursor position on same widget after re-render"
    (vui-defcomponent cursor-test ()
      :state ((label "initial"))
      :render
      (vui-vstack
        (vui-field :key "field1" :value "first")
        (vui-field :key "field2" :value "second")
        (vui-button "Update" :on-click (lambda ()
                                          (vui-set-state :label "changed")))))
    (let ((instance (vui-mount (vui-component 'cursor-test) "*test-cursor*")))
      (unwind-protect
          (with-current-buffer "*test-cursor*"
            ;; Move to second field widget
            (goto-char (point-min))
            (widget-forward 1)  ; first field
            (widget-forward 1)  ; second field
            (let ((pos-before (point)))
              ;; Verify we're at a widget
              (expect (widget-at (point)) :to-be-truthy)
              ;; Trigger re-render via button click
              (save-excursion
                (goto-char (point-min))
                (search-forward "[Update]")
                (backward-char 8)  ; position at start of "[Update]"
                (vui-test--click-button-at (point)))
              ;; Cursor should still be at a widget (field)
              (expect (widget-at (point)) :to-be-truthy)))
        (kill-buffer "*test-cursor*"))))

  (it "preserves cursor when content changes around widget"
    (vui-defcomponent cursor-content-test ()
      :state ((prefix "short"))
      :render
      (vui-vstack
        (vui-text prefix)
        (vui-field :key "input" :value "test")
        (vui-button "Toggle" :on-click
          (lambda ()
            (vui-set-state :prefix
              (if (string= prefix "short")
                  "this is a much longer prefix"
                "short"))))))
    (let ((instance (vui-mount (vui-component 'cursor-content-test) "*test-cursor2*")))
      (unwind-protect
          (with-current-buffer "*test-cursor2*"
            ;; Navigate to the field
            (goto-char (point-min))
            (widget-forward 1)  ; to field
            (let ((widget-before (widget-at (point))))
              (expect widget-before :to-be-truthy)
              ;; Change the prefix (content before widget changes length)
              (save-excursion
                (goto-char (point-min))
                (search-forward "[Toggle]")
                (backward-char 8)  ; position at start of "[Toggle]"
                (vui-test--click-button-at (point)))
              ;; Cursor should still be on a field widget
              (let ((widget-after (widget-at (point))))
                (expect widget-after :to-be-truthy)
                (expect (widget-type widget-after) :to-equal 'editable-field))))
        (kill-buffer "*test-cursor2*"))))

  (it "preserves cursor when widgets are added before current widget"
    ;; This tests the case where index-based tracking fails:
    ;; cursor is on widget at index 0, a new widget is inserted,
    ;; cursor should stay on original widget (now at index 1)
    (vui-defcomponent cursor-shift-test ()
      :state ((buttons nil))
      :render
      (vui-vstack
       (vui-text (format "Count: %d" (length buttons)))
       (vui-vstack
        (mapcar (lambda (id)
                  (vui-button (format "Button %d" id)
                    :key id
                    :on-click #'ignore))
                buttons))
       (vui-button "[Add]"
         :on-click (lambda ()
                     (vui-set-state :buttons
                       (append buttons (list (1+ (length buttons)))))))))
    (let ((instance (vui-mount (vui-component 'cursor-shift-test) "*test-cursor-shift*")))
      (unwind-protect
          (with-current-buffer "*test-cursor-shift*"
            ;; Find and position cursor on [Add] button
            (goto-char (point-min))
            (search-forward "[Add]")
            (backward-char 4)
            (let ((widget-before (widget-at (point))))
              (expect widget-before :to-be-truthy)
              ;; Click [Add] - this adds a button BEFORE [Add]
              (vui-test--click-button-at (point))
              (vui-flush-sync)
              ;; Cursor should still be on [Add], not on [Button 1]
              (let ((widget-after (widget-at (point))))
                (expect widget-after :to-be-truthy)
                ;; The widget should still be the [Add] button
                ;; Check :tag property (where vui stores button label)
                (expect (widget-get widget-after :tag) :to-match "Add"))))
        (kill-buffer "*test-cursor-shift*"))))

  (it "preserves cursor when widget type changes at same path"
    ;; Tests that cursor follows structural position even when widget type changes.
    ;; Path (1) is at index 1 in the vstack: Header is 0, the button/checkbox is 1.
    ;; The cursor should follow the path to the new widget type.
    (vui-defcomponent widget-type-change-test ()
      :state ((use-checkbox nil))
      :render
      (vui-vstack
       (vui-text "Header")
       (if use-checkbox
           (vui-checkbox :checked-p nil :label "Option")
         (vui-button "[Click me]" :on-click #'ignore))))
    (let ((instance (vui-mount (vui-component 'widget-type-change-test) "*test-type-change*")))
      (unwind-protect
          (with-current-buffer "*test-type-change*"
            ;; Position cursor on the button (path (1) - second child of vstack)
            (goto-char (point-min))
            (search-forward "[Click me]")
            (backward-char 5)
            (let* ((widget-before (widget-at (point)))
                   (path-before (widget-get widget-before :vui-path)))
              (expect widget-before :to-be-truthy)
              (expect path-before :to-equal '(1))
              ;; Change state with proper instance context (without moving cursor)
              (let ((vui--current-instance instance)
                    (vui--root-instance instance))
                (vui-set-state :use-checkbox t))
              (vui-flush-sync)
              ;; Cursor should now be on the checkbox (same path)
              (let* ((widget-after (widget-at (point)))
                     (path-after (when widget-after (widget-get widget-after :vui-path))))
                (expect widget-after :to-be-truthy)
                ;; Path should be the same
                (expect path-after :to-equal '(1))
                ;; Widget type should have changed to checkbox
                (expect (widget-type widget-after) :to-equal 'checkbox))))
        (kill-buffer "*test-type-change*"))))

  (it "caps cursor offset when widget shrinks"
    ;; Tests that cursor stays within bounds when widget becomes smaller.
    ;; Cursor at offset 15 in a 20-char field should cap to end of 5-char field.
    (vui-defcomponent widget-shrink-test ()
      :state ((large t))
      :render
      (vui-vstack
       (vui-field :value "test" :size (if large 20 5))))
    (let ((instance (vui-mount (vui-component 'widget-shrink-test) "*test-shrink*")))
      (unwind-protect
          (with-current-buffer "*test-shrink*"
            ;; Position cursor deep in the field (offset ~15 from start)
            (goto-char (point-min))
            (let* ((widget-before (widget-at (point)))
                   (bounds-before (vui--widget-bounds widget-before))
                   (field-start (car bounds-before)))
              (expect widget-before :to-be-truthy)
              ;; Move to offset 15 within the field
              (goto-char (+ field-start 15))
              (expect (widget-at (point)) :to-equal widget-before)
              ;; Shrink the field
              (let ((vui--current-instance instance)
                    (vui--root-instance instance))
                (vui-set-state :large nil))
              (vui-flush-sync)
              ;; Cursor should be on the (smaller) field, capped to its bounds
              (let* ((widget-after (widget-at (point)))
                     (bounds-after (vui--widget-bounds widget-after))
                     (new-start (car bounds-after))
                     (new-end (cdr bounds-after)))
                (expect widget-after :to-be-truthy)
                (expect (widget-type widget-after) :to-equal 'editable-field)
                ;; Cursor should be within the new smaller bounds [start, end)
                (expect (>= (point) new-start) :to-be-truthy)
                (expect (< (point) new-end) :to-be-truthy))))
        (kill-buffer "*test-shrink*")))))

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


;;; vui-component-test.el ends here

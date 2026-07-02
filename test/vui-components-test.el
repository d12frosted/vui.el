;;; vui-components-test.el --- Tests for vui-components.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui-components.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)
(require 'vui-components)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-components-test--click-button-at (pos)
  "Invoke the button widget at POS.
Buttons are widget.el push-buttons, so we use widget-apply."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

(defun vui-components-test--widget-by-tag (tag)
  "Return the first buffer widget whose :tag is TAG, or nil."
  (seq-find (lambda (w) (equal (widget-get w :tag) tag))
            (vui--collect-widgets)))

(describe "vui type conversion functions"
  (describe "vui--field-value-to-string"
    (it "returns empty string for nil value with type"
      (expect (vui--field-value-to-string nil 'integer) :to-equal ""))

    (it "returns value as-is when type is nil"
      (expect (vui--field-value-to-string "hello" nil) :to-equal "hello")
      (expect (vui--field-value-to-string nil nil) :to-equal ""))

    (it "converts integer to string"
      (expect (vui--field-value-to-string 42 'integer) :to-equal "42")
      (expect (vui--field-value-to-string -5 'integer) :to-equal "-5"))

    (it "converts float to string"
      (expect (vui--field-value-to-string 3.14 'float) :to-equal "3.14"))

    (it "converts symbol to string"
      (expect (vui--field-value-to-string 'foo 'symbol) :to-equal "foo"))

    (it "converts sexp to string"
      (expect (vui--field-value-to-string '(a b c) 'sexp) :to-equal "(a b c)")))

  (describe "vui--field-parse-string"
    (it "returns string unchanged when type is nil"
      (let ((result (vui--field-parse-string "hello" nil)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal "hello")))

    (it "parses valid integer"
      (let ((result (vui--field-parse-string "42" 'integer)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 42)))

    (it "parses negative integer"
      (let ((result (vui--field-parse-string "-5" 'integer)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal -5)))

    (it "parses empty string as nil for numeric types"
      ;; This allows clearing a field to type a new number
      (let ((int-result (vui--field-parse-string "" 'integer))
            (nat-result (vui--field-parse-string "  " 'natnum))
            (float-result (vui--field-parse-string "" 'float))
            (num-result (vui--field-parse-string "" 'number)))
        (expect (car int-result) :to-equal 'ok)
        (expect (cdr int-result) :to-be nil)
        (expect (car nat-result) :to-equal 'ok)
        (expect (cdr nat-result) :to-be nil)
        (expect (car float-result) :to-equal 'ok)
        (expect (cdr float-result) :to-be nil)
        (expect (car num-result) :to-equal 'ok)
        (expect (cdr num-result) :to-be nil)))

    (it "rejects non-integer for integer type"
      (let ((result (vui--field-parse-string "abc" 'integer)))
        (expect (car result) :to-equal 'error)))

    (it "rejects float for integer type"
      (let ((result (vui--field-parse-string "3.14" 'integer)))
        (expect (car result) :to-equal 'error)))

    (it "parses valid natnum"
      (let ((result (vui--field-parse-string "0" 'natnum)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 0)))

    (it "rejects negative for natnum type"
      (let ((result (vui--field-parse-string "-5" 'natnum)))
        (expect (car result) :to-equal 'error)))

    (it "parses valid float"
      (let ((result (vui--field-parse-string "3.14" 'float)))
        (expect (car result) :to-equal 'ok)
        (expect (floatp (cdr result)) :to-be-truthy)))

    (it "accepts integer input for float type without coercion"
      (let ((result (vui--field-parse-string "42" 'float)))
        (expect (car result) :to-equal 'ok)
        (expect (integerp (cdr result)) :to-be-truthy)
        (expect (cdr result) :to-equal 42)))

    (it "treats trailing decimal as integer for clean backspace"
      (let ((result (vui--field-parse-string "42." 'float)))
        (expect (car result) :to-equal 'ok)
        (expect (integerp (cdr result)) :to-be-truthy)
        (expect (cdr result) :to-equal 42)))

    (it "accepts leading decimal for float type"
      (let ((result (vui--field-parse-string ".5" 'float)))
        (expect (car result) :to-equal 'ok)
        (expect (floatp (cdr result)) :to-be-truthy)
        (expect (cdr result) :to-equal 0.5)))

    (it "parses valid number (integer)"
      (let ((result (vui--field-parse-string "42" 'number)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 42)))

    (it "parses valid number (float)"
      (let ((result (vui--field-parse-string "3.14" 'number)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 3.14)))

    (it "expands file path with expand-file-name"
      (let ((result (vui--field-parse-string "~/test.txt" 'file)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal (expand-file-name "~/test.txt"))))

    (it "expands directory path with expand-file-name"
      (let ((result (vui--field-parse-string "~/Documents" 'directory)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal (expand-file-name "~/Documents"))))

    (it "returns nil for empty file path"
      (let ((result (vui--field-parse-string "" 'file)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-be nil)))

    (it "returns nil for empty directory path"
      (let ((result (vui--field-parse-string "  " 'directory)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-be nil)))

    (it "parses symbol"
      (let ((result (vui--field-parse-string "my-symbol" 'symbol)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 'my-symbol)))

    (it "rejects empty string for symbol"
      (let ((result (vui--field-parse-string "  " 'symbol)))
        (expect (car result) :to-equal 'error)))

    (it "parses sexp"
      (let ((result (vui--field-parse-string "(1 2 3)" 'sexp)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal '(1 2 3))))

    (it "handles whitespace in input"
      (let ((result (vui--field-parse-string "  42  " 'integer)))
        (expect (car result) :to-equal 'ok)
        (expect (cdr result) :to-equal 42))))

  (describe "vui--typed-field-validate"
    (it "validates :min constraint"
      (let ((err (vui--typed-field-validate 5 'integer 10 nil nil nil nil nil "5")))
        (expect err :to-match "at least 10")))

    (it "validates :max constraint"
      (let ((err (vui--typed-field-validate 100 'integer nil 50 nil nil nil nil "100")))
        (expect err :to-match "at most 50")))

    (it "validates :required constraint"
      (let ((err (vui--typed-field-validate nil 'integer nil nil nil t nil nil "  ")))
        (expect err :to-match "required")))

    (it "passes when constraints are met"
      (let ((err (vui--typed-field-validate 25 'integer 10 50 nil nil nil nil "25")))
        (expect err :to-be nil)))

    (it "calls custom validator with typed value"
      (let* ((received-value nil)
             (validator (lambda (v)
                          (setq received-value v)
                          (when (cl-oddp v) "Must be even")))
             (err (vui--typed-field-validate 5 'integer nil nil validator nil nil nil "5")))
        (expect received-value :to-equal 5)
        (expect err :to-equal "Must be even")))

    (it "validates :must-exist for file type"
      (let ((err (vui--typed-field-validate "/nonexistent/file.txt" 'file
                                             nil nil nil nil t nil "/nonexistent/file.txt")))
        (expect err :to-match "File does not exist")))

    (it "validates :must-exist for directory type"
      (let ((err (vui--typed-field-validate "/nonexistent/dir" 'directory
                                             nil nil nil nil t nil "/nonexistent/dir")))
        (expect err :to-match "Directory does not exist")))

    (it "passes :must-exist when file exists"
      (let ((err (vui--typed-field-validate (expand-file-name "vui.el") 'file
                                             nil nil nil nil t nil "vui.el")))
        (expect err :to-be nil)))

    (it "passes :must-exist when directory exists"
      (let ((err (vui--typed-field-validate (expand-file-name "test") 'directory
                                             nil nil nil nil t nil "test")))
        (expect err :to-be nil)))

    (it "skips :must-exist check for nil value"
      (let ((err (vui--typed-field-validate nil 'file nil nil nil nil t nil "")))
        (expect err :to-be nil)))

    (it "validates :extensions for file type"
      (let ((err (vui--typed-field-validate "/path/to/file.txt" 'file
                                             nil nil nil nil nil '("el" "org") "/path/to/file.txt")))
        (expect err :to-match "Must be a .el or .org file")))

    (it "passes :extensions when extension matches"
      (let ((err (vui--typed-field-validate "/path/to/file.el" 'file
                                             nil nil nil nil nil '("el" "org") "/path/to/file.el")))
        (expect err :to-be nil)))

    (it "skips :extensions check for nil value"
      (let ((err (vui--typed-field-validate nil 'file nil nil nil nil nil '("el") "")))
        (expect err :to-be nil)))))

(describe "vui-collapsible"
  (describe "basic structure"
    (it "creates a component vnode"
      (let ((node (vui-collapsible :title "Test"
                    (vui-text "Content"))))
        (expect (vui-vnode-component-p node) :to-be-truthy)
        (expect (vui-vnode-component-type node) :to-equal 'vui-collapsible--internal)))

    (it "passes title prop"
      (let ((node (vui-collapsible :title "My Section"
                    (vui-text "Content"))))
        (expect (plist-get (vui-vnode-component-props node) :title)
                :to-equal "My Section")))

    (it "passes children"
      (let ((node (vui-collapsible :title "Section"
                    (vui-text "Child 1")
                    (vui-text "Child 2"))))
        (expect (length (vui-vnode-component-children node)) :to-equal 2))))

  (describe "rendering collapsed state"
    (it "shows collapsed indicator by default"
      (vui-defcomponent collapsible-test-1 ()
        :render
        (vui-collapsible :title "Section"
          (vui-text "Hidden content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-1)
                                  "*test-collapsible-1*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-1*"
              ;; Should show collapsed indicator
              (expect (buffer-string) :to-match "▶ Section")
              ;; Content should NOT be visible
              (expect (buffer-string) :not :to-match "Hidden content"))
          (kill-buffer "*test-collapsible-1*"))))

    (it "hides content when collapsed"
      (vui-defcomponent collapsible-test-2 ()
        :render
        (vui-collapsible :title "Test"
          (vui-text "Secret")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-2)
                                  "*test-collapsible-2*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-2*"
              (expect (buffer-string) :not :to-match "Secret"))
          (kill-buffer "*test-collapsible-2*")))))

  (describe "rendering expanded state"
    (it "shows expanded indicator when initially-expanded"
      (vui-defcomponent collapsible-test-3 ()
        :render
        (vui-collapsible :title "Section" :initially-expanded t
          (vui-text "Visible content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-3)
                                  "*test-collapsible-3*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-3*"
              ;; Should show expanded indicator
              (expect (buffer-string) :to-match "▼ Section")
              ;; Content should be visible
              (expect (buffer-string) :to-match "Visible content"))
          (kill-buffer "*test-collapsible-3*"))))

    (it "indents content when expanded"
      (vui-defcomponent collapsible-test-4 ()
        :render
        (vui-collapsible :title "Section" :initially-expanded t
          (vui-text "Indented")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-4)
                                  "*test-collapsible-4*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-4*"
              ;; Content should be on second line with default indent (2 spaces)
              (goto-char (point-min))
              (forward-line 1)
              (expect (looking-at "  Indented") :to-be-truthy))
          (kill-buffer "*test-collapsible-4*")))))

  (describe "toggle behavior"
    (it "expands when header is clicked"
      (vui-defcomponent collapsible-test-5 ()
        :render
        (vui-collapsible :title "Clickable"
          (vui-text "Now visible")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-5)
                                  "*test-collapsible-5*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-5*"
              ;; Initially collapsed
              (expect (buffer-string) :not :to-match "Now visible")
              ;; Click the header
              (goto-char (point-min))
              (vui-components-test--click-button-at (point))
              ;; Flush deferred render
              (vui-flush-sync)
              ;; Now expanded
              (expect (buffer-string) :to-match "Now visible")
              (expect (buffer-string) :to-match "▼ Clickable"))
          (kill-buffer "*test-collapsible-5*"))))

    (it "collapses when header is clicked again"
      (vui-defcomponent collapsible-test-6 ()
        :render
        (vui-collapsible :title "Toggle" :initially-expanded t
          (vui-text "Content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-6)
                                  "*test-collapsible-6*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-6*"
              ;; Initially expanded
              (expect (buffer-string) :to-match "Content")
              ;; Click to collapse
              (goto-char (point-min))
              (vui-components-test--click-button-at (point))
              ;; Flush deferred render
              (vui-flush-sync)
              ;; Now collapsed
              (expect (buffer-string) :not :to-match "Content")
              (expect (buffer-string) :to-match "▶ Toggle"))
          (kill-buffer "*test-collapsible-6*")))))

  (describe "controlled mode"
    (it "respects :expanded prop"
      (vui-defcomponent collapsible-test-7 ()
        :render
        (vui-collapsible :title "Controlled" :expanded t
          (vui-text "Controlled content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-7)
                                  "*test-collapsible-7*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-7*"
              (expect (buffer-string) :to-match "Controlled content")
              (expect (buffer-string) :to-match "▼ Controlled"))
          (kill-buffer "*test-collapsible-7*"))))

    (it "respects :expanded nil prop"
      (vui-defcomponent collapsible-test-8 ()
        :render
        (vui-collapsible :title "Controlled" :expanded nil
          (vui-text "Hidden")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-8)
                                  "*test-collapsible-8*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-8*"
              (expect (buffer-string) :not :to-match "Hidden")
              (expect (buffer-string) :to-match "▶ Controlled"))
          (kill-buffer "*test-collapsible-8*"))))

    (it "calls :on-toggle with new state"
      (let ((toggled-state nil))
        (vui-defcomponent collapsible-test-9 ()
          :render
          (vui-collapsible :title "Callback"
                           :on-toggle (lambda (expanded)
                                        (setq toggled-state expanded))
            (vui-text "Content")))
        (let ((instance (vui-mount (vui-component 'collapsible-test-9)
                                    "*test-collapsible-9*")))
          (unwind-protect
              (with-current-buffer "*test-collapsible-9*"
                ;; Click to expand
                (goto-char (point-min))
                (vui-components-test--click-button-at (point))
                (vui-flush-sync)
                (expect toggled-state :to-be t)
                ;; Click to collapse
                (goto-char (point-min))
                (vui-components-test--click-button-at (point))
                (vui-flush-sync)
                (expect toggled-state :to-be nil))
            (kill-buffer "*test-collapsible-9*"))))))

  (describe "custom indicators"
    (it "uses custom expanded indicator"
      (vui-defcomponent collapsible-test-10 ()
        :render
        (vui-collapsible :title "Custom"
                         :initially-expanded t
                         :expanded-indicator "[-]"
          (vui-text "Content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-10)
                                  "*test-collapsible-10*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-10*"
              (expect (buffer-string) :to-match "\\[-\\] Custom"))
          (kill-buffer "*test-collapsible-10*"))))

    (it "uses custom collapsed indicator"
      (vui-defcomponent collapsible-test-11 ()
        :render
        (vui-collapsible :title "Custom"
                         :collapsed-indicator "[+]"
          (vui-text "Content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-11)
                                  "*test-collapsible-11*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-11*"
              (expect (buffer-string) :to-match "\\[\\+\\] Custom"))
          (kill-buffer "*test-collapsible-11*")))))

  (describe "custom title-face"
    (it "applies face to header"
      (vui-defcomponent collapsible-test-12 ()
        :render
        (vui-collapsible :title "Bold" :title-face 'bold
          (vui-text "Content")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-12)
                                  "*test-collapsible-12*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-12*"
              ;; The face is applied via overlay (widget.el uses overlays)
              (goto-char (point-min))
              (let* ((ovs (overlays-at (point)))
                     (face (when ovs (overlay-get (car ovs) 'face))))
                ;; Face can be 'bold or include 'bold in a list
                (expect (or (eq face 'bold)
                            (and (listp face) (memq 'bold face)))
                        :to-be-truthy)))
          (kill-buffer "*test-collapsible-12*")))))

  (describe "custom indent"
    (it "uses custom indent level"
      (vui-defcomponent collapsible-test-13 ()
        :render
        (vui-collapsible :title "Section"
                         :initially-expanded t
                         :indent 4
          (vui-text "Indented")))
      (let ((instance (vui-mount (vui-component 'collapsible-test-13)
                                  "*test-collapsible-13*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-13*"
              ;; Content should be indented 4 spaces
              (goto-char (point-min))
              (forward-line 1)
              (expect (looking-at "    Indented") :to-be-truthy))
          (kill-buffer "*test-collapsible-13*")))))

  (describe "multiple collapsibles"
    (it "work independently"
      (vui-defcomponent collapsible-test-14 ()
        :render
        (vui-vstack
         (vui-collapsible :title "First" :key 'first
           (vui-text "First content"))
         (vui-collapsible :title "Second" :key 'second
           (vui-text "Second content"))))
      (let ((instance (vui-mount (vui-component 'collapsible-test-14)
                                  "*test-collapsible-14*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-14*"
              ;; Both collapsed initially
              (expect (buffer-string) :not :to-match "First content")
              (expect (buffer-string) :not :to-match "Second content")
              ;; Click first header
              (goto-char (point-min))
              (vui-components-test--click-button-at (point))
              (vui-flush-sync)
              ;; First expanded, second still collapsed
              (expect (buffer-string) :to-match "First content")
              (expect (buffer-string) :not :to-match "Second content"))
          (kill-buffer "*test-collapsible-14*")))))

  (describe "nested collapsibles"
    (it "work correctly"
      (vui-defcomponent collapsible-test-15 ()
        :render
        (vui-collapsible :title "Outer" :initially-expanded t
          (vui-collapsible :title "Inner" :key 'inner
            (vui-text "Deep content"))))
      (let ((instance (vui-mount (vui-component 'collapsible-test-15)
                                  "*test-collapsible-15*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-15*"
              ;; Outer expanded, inner collapsed
              (expect (buffer-string) :to-match "▼ Outer")
              (expect (buffer-string) :to-match "▶ Inner")
              (expect (buffer-string) :not :to-match "Deep content"))
          (kill-buffer "*test-collapsible-15*"))))

    (it "propagates indentation correctly"
      (vui-defcomponent collapsible-test-16 ()
        :render
        (vui-collapsible :title "Outer" :initially-expanded t
          (vui-text "Outer content")
          (vui-collapsible :title "Inner" :initially-expanded t :key 'inner
            (vui-text "Inner content"))))
      (let ((instance (vui-mount (vui-component 'collapsible-test-16)
                                  "*test-collapsible-16*")))
        (unwind-protect
            (with-current-buffer "*test-collapsible-16*"
              ;; Check line by line indentation
              (goto-char (point-min))
              ;; Line 1: "▼ Outer" at column 0
              (expect (looking-at "▼ Outer") :to-be-truthy)
              (forward-line 1)
              ;; Line 2: "  Outer content" at column 2
              (expect (looking-at "  Outer content") :to-be-truthy)
              (forward-line 1)
              ;; Line 3: "  ▼ Inner" at column 2
              (expect (looking-at "  ▼ Inner") :to-be-truthy)
              (forward-line 1)
              ;; Line 4: "    Inner content" at column 4 (accumulated indent)
              (expect (looking-at "    Inner content") :to-be-truthy))
          (kill-buffer "*test-collapsible-16*")))))

  (describe "cursor identity across expand and collapse"
    ;; The header toggle bakes the ▶/▼ indicator into its label, so the
    ;; label flips on every toggle.  When a re-render also shifts the
    ;; toggle down (a row appears above it in the same render), neither the
    ;; ordinal path nor the widget index still points at it, and the
    ;; flipped label defeats label-based cursor identity: point drifts onto
    ;; the new row.  A stable :key on the toggle, derived from the title or
    ;; passed by the caller, keeps point on it.

    (it "keeps point on the toggle when a row shifts in as it expands"
      (let ((vui-render-delay nil))
        (vui-defcomponent cc-toggle-shift ()
          :state ((open nil))
          :render
          (vui-vstack
           ;; Appears in the SAME render that expands the section, so the
           ;; toggle both moves down and flips its indicator at once.
           (when open (vui-button "banner"))
           (vui-collapsible :title "Details" :expanded open
             (vui-text "body"))))
        (let ((inst (vui-mount (vui-component 'cc-toggle-shift) "*cc-ts*")))
          (unwind-protect
              (with-current-buffer "*cc-ts*"
                (expect (buffer-string) :to-equal "▶ Details")
                (goto-char (car (vui--widget-bounds
                                 (vui-components-test--widget-by-tag
                                  "▶ Details"))))
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▶ Details")
                (let ((vui--current-instance inst)) (vui-set-state :open t))
                ;; Row inserted above AND indicator flipped in one render;
                ;; point must ride the toggle, not drift onto "banner".
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▼ Details"))
            (kill-buffer "*cc-ts*")))))

    (it "keeps point on the right toggle via :key when titles repeat"
      (let ((vui-render-delay nil))
        (vui-defcomponent cc-toggle-dup ()
          :state ((open nil))
          :render
          (vui-vstack
           (when open (vui-button "banner"))
           (vui-collapsible :title "Item" :key 'a
             (vui-text "a-body"))
           (vui-collapsible :title "Item" :key 'b :expanded open
             (vui-text "b-body"))))
        (let ((inst (vui-mount (vui-component 'cc-toggle-dup) "*cc-dup*")))
          (unwind-protect
              (with-current-buffer "*cc-dup*"
                ;; Park on the SECOND "Item" toggle (key b); the two share a
                ;; title, so only the caller's :key tells them apart.
                (let ((toggles (seq-filter
                                (lambda (w)
                                  (equal (widget-get w :tag) "▶ Item"))
                                (vui--collect-widgets))))
                  (goto-char (car (vui--widget-bounds (nth 1 toggles)))))
                (let ((vui--current-instance inst)) (vui-set-state :open t))
                ;; Banner shifts both toggles down and the second's indicator
                ;; flips; without the caller's :key threaded to the toggle,
                ;; point drifts onto the first "Item".
                (expect (widget-get (widget-at (point)) :vui-key)
                        :to-equal 'b)
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▼ Item"))
            (kill-buffer "*cc-dup*")))))

    (it "keeps point on a sibling toggle when the section above it expands"
      ;; Regression guard: expanding the FIRST collapsible inserts content
      ;; above the SECOND.  The second's label does not change, so this
      ;; already works via the stable path; the toggle key must not
      ;; regress it.
      (let ((vui-render-delay nil))
        (vui-defcomponent cc-sibling ()
          :render
          (vui-vstack
           (vui-collapsible :title "Foo"
             (vui-button "inside-foo"))
           (vui-collapsible :title "Bar"
             (vui-button "inside-bar"))))
        (let ((inst (vui-mount (vui-component 'cc-sibling) "*cc-sib*")))
          (ignore inst)
          (unwind-protect
              (with-current-buffer "*cc-sib*"
                (goto-char (car (vui--widget-bounds
                                 (vui-components-test--widget-by-tag
                                  "▶ Bar"))))
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▶ Bar")
                ;; Expand Foo by invoking its header toggle.
                (widget-apply (vui-components-test--widget-by-tag "▶ Foo")
                              :action)
                (vui-flush-sync)
                (expect (buffer-string) :to-match "inside-foo")
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▶ Bar"))
            (kill-buffer "*cc-sib*"))))))

  (describe "rich header"
    (it "right-aligns :header-right at :header-width via space-between"
      (vui-defcomponent cc-rich-align ()
        :render
        (vui-collapsible :title "Schema"
                         :header-width 20
                         :header-right (vui-text "3 invalid")
          (vui-text "body")))
      (let ((instance (vui-mount (vui-component 'cc-rich-align)
                                 "*cc-rich-align*")))
        (ignore instance)
        (unwind-protect
            (with-current-buffer "*cc-rich-align*"
              (goto-char (point-min))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                ;; Toggle flush left, right content flush to header-width.
                (expect line :to-match "\\`▶ Schema")
                (expect line :to-match "3 invalid\\'")
                (expect (string-width line) :to-equal 20)))
          (kill-buffer "*cc-rich-align*"))))

    (it "keeps :header-right visible in the header row when collapsed"
      ;; It is header adornment, not a child: shown even while the body
      ;; is hidden.
      (vui-defcomponent cc-rich-collapsed ()
        :render
        (vui-collapsible :title "Schema"
                         :header-width 30
                         :header-right (vui-text "3 invalid")
          (vui-text "secret-body")))
      (let ((instance (vui-mount (vui-component 'cc-rich-collapsed)
                                 "*cc-rich-collapsed*")))
        (ignore instance)
        (unwind-protect
            (with-current-buffer "*cc-rich-collapsed*"
              (expect (buffer-string) :to-match "▶ Schema")
              (expect (buffer-string) :to-match "3 invalid")
              (expect (buffer-string) :not :to-match "secret-body"))
          (kill-buffer "*cc-rich-collapsed*"))))

    (it "toggles and keeps point on the toggle with a rich header"
      (let ((vui-render-delay nil))
        (vui-defcomponent cc-rich-toggle ()
          :render
          (vui-collapsible :title "Schema"
                           :header-width 30
                           :header-right (vui-text "3 invalid")
            (vui-text "body")))
        (let ((instance (vui-mount (vui-component 'cc-rich-toggle)
                                   "*cc-rich-toggle*")))
          (ignore instance)
          (unwind-protect
              (with-current-buffer "*cc-rich-toggle*"
                (expect (buffer-string) :not :to-match "body")
                (goto-char (car (vui--widget-bounds
                                 (vui-components-test--widget-by-tag
                                  "▶ Schema"))))
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▶ Schema")
                (vui-components-test--click-button-at (point))
                (vui-flush-sync)
                ;; Expanded: body visible, right content still in the header.
                (expect (buffer-string) :to-match "body")
                (expect (buffer-string) :to-match "3 invalid")
                ;; Point rode the toggle across the label flip (#103).
                (expect (widget-get (widget-at (point)) :tag)
                        :to-equal "▼ Schema"))
            (kill-buffer "*cc-rich-toggle*")))))

    (it "defaults :header-width to fill-column"
      (vui-defcomponent cc-rich-default ()
        :render
        (vui-collapsible :title "Schema"
                         :header-right (vui-text "9")
          (vui-text "body")))
      (let ((instance (vui-mount (vui-component 'cc-rich-default)
                                 "*cc-rich-default*")))
        (ignore instance)
        (unwind-protect
            (with-current-buffer "*cc-rich-default*"
              (goto-char (point-min))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (expect (string-width line) :to-equal fill-column)))
          (kill-buffer "*cc-rich-default*"))))

    (it "leaves the header unchanged when :header-right is absent"
      ;; No flex wrapper, no padding: the header is exactly the toggle.
      (vui-defcomponent cc-plain-header ()
        :render
        (vui-collapsible :title "Section"
          (vui-text "body")))
      (let ((instance (vui-mount (vui-component 'cc-plain-header)
                                 "*cc-plain-header*")))
        (ignore instance)
        (unwind-protect
            (with-current-buffer "*cc-plain-header*"
              (expect (buffer-string) :to-equal "▶ Section"))
          (kill-buffer "*cc-plain-header*"))))

    (it "indents a nested collapsible's rich header correctly"
      (vui-defcomponent cc-rich-nested ()
        :render
        (vui-collapsible :title "Outer" :initially-expanded t
          (vui-collapsible :title "Inner" :initially-expanded t
                           :header-width 20
                           :header-right (vui-text "9")
            (vui-text "deep"))))
      (let ((instance (vui-mount (vui-component 'cc-rich-nested)
                                 "*cc-rich-nested*")))
        (ignore instance)
        (unwind-protect
            (with-current-buffer "*cc-rich-nested*"
              (goto-char (point-min))
              (expect (looking-at "▼ Outer") :to-be-truthy)
              (forward-line 1)
              ;; Line 2 is the inner rich header, indented by 2.
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (expect line :to-match "\\`  ▼ Inner")
                (expect line :to-match "9\\'")
                (expect (string-width line) :to-equal 20)))
          (kill-buffer "*cc-rich-nested*"))))))

(defun vui-components-test--placeholder-overlays ()
  "Return all placeholder overlays in the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'vui-placeholder))
              (overlays-in (point-min) (point-max))))

(describe "vui-typed-field"
  (describe "placeholder"
    (it "shows :placeholder while the field is empty"
      (with-temp-buffer
        (vui-render (vui-typed-field :type 'integer :size 8
                                     :placeholder "0-100"))
        (let ((overlays (vui-components-test--placeholder-overlays)))
          (expect (length overlays) :to-equal 1)
          (expect (overlay-get (car overlays) 'display) :to-match "0-100"))))

    (it "does not show :placeholder when the field has a value"
      (with-temp-buffer
        (vui-render (vui-typed-field :type 'integer :size 8 :value 42
                                     :placeholder "0-100"))
        (expect (vui-components-test--placeholder-overlays) :to-equal nil)))

    (it "works through the typed shortcuts"
      (with-temp-buffer
        (vui-render (vui-integer-field :size 8 :placeholder "count"))
        (expect (length (vui-components-test--placeholder-overlays))
                :to-equal 1))))

  (describe "vnode creation"
    (it "creates a component vnode"
      (let ((node (vui-typed-field :type 'integer :value 42)))
        (expect (vui-vnode-component-p node) :to-be-truthy)
        (expect (vui-vnode-component-type node) :to-equal 'vui-typed-field--internal)))

    (it "passes type prop"
      (let* ((node (vui-typed-field :type 'integer :value 42))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type)))
        (expect type-val :to-equal 'integer)))

    (it "passes value prop"
      (let ((node (vui-typed-field :type 'integer :value 42)))
        (expect (plist-get (vui-vnode-component-props node) :value) :to-equal 42)))

    (it "passes all props"
      (let ((node (vui-typed-field :type 'integer :value 10 :min 0 :max 100 :key 'age)))
        (expect (plist-get (vui-vnode-component-props node) :min) :to-equal 0)
        (expect (plist-get (vui-vnode-component-props node) :max) :to-equal 100)
        (expect (plist-get (vui-vnode-component-props node) :key) :to-equal 'age))))

  (describe "rendering"
    (it "displays value converted to string"
      (vui-defcomponent typed-field-render-test ()
        :render
        (vui-typed-field :type 'integer :value 42 :key 'test-field))
      (let ((instance (vui-mount (vui-component 'typed-field-render-test)
                                  "*test-typed-field-render*")))
        (unwind-protect
            (with-current-buffer "*test-typed-field-render*"
              (expect (buffer-string) :to-match "42"))
          (kill-buffer "*test-typed-field-render*"))))

    (it "displays float value"
      (vui-defcomponent typed-field-float-test ()
        :render
        (vui-typed-field :type 'float :value 3.14 :key 'test-field))
      (let ((instance (vui-mount (vui-component 'typed-field-float-test)
                                  "*test-typed-field-float*")))
        (unwind-protect
            (with-current-buffer "*test-typed-field-float*"
              (expect (buffer-string) :to-match "3.14"))
          (kill-buffer "*test-typed-field-float*")))))

  (describe "callback integration"
    (it "calls on-change with typed integer value"
      (let ((received nil))
        (vui-defcomponent typed-change-test ()
          :render
          (vui-typed-field :type 'integer
                           :value 0
                           :key 'test-field
                           :on-change (lambda (v) (setq received v))))
        (let ((instance (vui-mount (vui-component 'typed-change-test)
                                    "*test-typed-change*")))
          (unwind-protect
              (with-current-buffer "*test-typed-change*"
                (let ((widget (car widget-field-list)))
                  ;; Simulate typing a number
                  (widget-value-set widget "42")
                  ;; Trigger notify
                  (widget-apply widget :notify widget))
                ;; Flush any deferred renders
                (vui-flush-sync)
                ;; Check that we received an integer, not a string
                (expect (integerp received) :to-be-truthy)
                (expect received :to-equal 42))
            (kill-buffer "*test-typed-change*")))))

    (it "calls on-error for invalid input"
      (let ((error-received nil))
        (vui-defcomponent typed-error-test ()
          :render
          (vui-typed-field :type 'integer
                           :value 0
                           :key 'test-field
                           :on-error (lambda (err _raw) (setq error-received err))
                           :on-change #'identity))
        (let ((instance (vui-mount (vui-component 'typed-error-test)
                                    "*test-typed-error*")))
          (unwind-protect
              (with-current-buffer "*test-typed-error*"
                (let ((widget (car widget-field-list)))
                  ;; Simulate typing invalid input
                  (widget-value-set widget "abc")
                  (widget-apply widget :notify widget))
                ;; Flush deferred renders
                (vui-flush-sync)
                ;; Should have received an error
                (expect error-received :to-be-truthy)
                (expect error-received :to-match "integer"))
            (kill-buffer "*test-typed-error*")))))

    (it "validates :min and calls on-error"
      (let ((error-received nil))
        (vui-defcomponent typed-min-test ()
          :render
          (vui-typed-field :type 'integer
                           :value 50
                           :min 10
                           :key 'test-field
                           :on-error (lambda (err _raw) (setq error-received err))
                           :on-change #'identity))
        (let ((instance (vui-mount (vui-component 'typed-min-test)
                                    "*test-typed-min*")))
          (unwind-protect
              (with-current-buffer "*test-typed-min*"
                (let ((widget (car widget-field-list)))
                  ;; Simulate typing value below min
                  (widget-value-set widget "5")
                  (widget-apply widget :notify widget))
                (vui-flush-sync)
                (expect error-received :to-match "at least 10"))
            (kill-buffer "*test-typed-min*")))))

    (it "calls on-error with nil when input recovers to valid"
      ;; The clear signal: after reporting an error, recovering to valid input
      ;; must notify on-error with nil so listeners tracking errors (like the
      ;; form examples, which gate submit on a `null errors' check) can clear
      ;; them.  Without this a transient error (e.g. a half-typed email) sticks
      ;; forever and the submit button stays disabled.
      (let ((calls '()))
        (vui-defcomponent typed-recover-test ()
          :render
          (vui-typed-field :type 'integer
                           :value 0
                           :min 10
                           :key 'test-field
                           :on-error (lambda (err _raw) (push err calls))
                           :on-change #'identity))
        (let ((instance (vui-mount (vui-component 'typed-recover-test)
                                    "*test-typed-recover*")))
          (unwind-protect
              (with-current-buffer "*test-typed-recover*"
                ;; below min -> error.  Re-fetch the field before each input:
                ;; a re-render recreates the widget, and real typing always
                ;; goes through the current one.
                (let ((widget (car widget-field-list)))
                  (widget-value-set widget "5")
                  (widget-apply widget :notify widget)
                  (vui-flush-sync))
                (let ((widget (car widget-field-list)))
                  ;; valid -> should clear (on-error called with nil)
                  (widget-value-set widget "20")
                  (widget-apply widget :notify widget)
                  (vui-flush-sync))
                ;; most recent on-error call cleared the error...
                (expect (car calls) :to-be nil)
                ;; ...and there was a real error before it
                (expect (seq-some #'identity calls) :to-be-truthy))
            (kill-buffer "*test-typed-recover*")))))

    (it "does not call on-change when validation fails"
      (let ((change-received nil)
            (error-received nil))
        (vui-defcomponent typed-validation-ux-test ()
          :render
          (vui-typed-field :type 'integer
                           :value 1900
                           :min 1900
                           :key 'test-field
                           :on-error (lambda (err _raw) (setq error-received err))
                           :on-change (lambda (v) (setq change-received v))))
        (let ((instance (vui-mount (vui-component 'typed-validation-ux-test)
                                    "*test-typed-validation-ux*")))
          (unwind-protect
              (with-current-buffer "*test-typed-validation-ux*"
                (let ((widget (car widget-field-list)))
                  ;; Simulate typing intermediate value "1" (below min 1900)
                  (widget-value-set widget "1")
                  (widget-apply widget :notify widget))
                (vui-flush-sync)
                ;; on-change should NOT be called when validation fails
                (expect change-received :to-be nil)
                ;; on-error SHOULD be called
                (expect error-received :to-match "at least 1900"))
            (kill-buffer "*test-typed-validation-ux*"))))))

  (describe "raw input preservation"
    (it "preserves invalid input across re-renders"
      (let ((error-received nil))
        (vui-defcomponent typed-preserve-test ()
          :state ((counter 0))
          :render
          (vui-vstack
            (vui-typed-field :type 'integer
                             :value 0
                             :key 'test-field
                             :on-error (lambda (err _raw) (setq error-received err))
                             :on-change #'identity)
            (vui-button "Increment"
              :key 'btn
              :on-click (lambda ()
                          (vui-set-state :counter (1+ counter))))))
        (let ((instance (vui-mount (vui-component 'typed-preserve-test)
                                    "*test-typed-preserve*")))
          (unwind-protect
              (with-current-buffer "*test-typed-preserve*"
                (let ((widget (car widget-field-list)))
                  ;; Type invalid input
                  (widget-value-set widget "123f")
                  (widget-apply widget :notify widget))
                (vui-flush-sync)
                ;; Error should be set
                (expect error-received :to-be-truthy)
                ;; Click button to trigger re-render
                (goto-char (point-min))
                (search-forward "[Increment]")
                (vui-components-test--click-button-at (- (point) 5))
                (vui-flush-sync)
                ;; Check that the invalid input is preserved
                (let ((widget (car widget-field-list)))
                  (expect (widget-value widget) :to-equal "123f")))
            (kill-buffer "*test-typed-preserve*")))))

    (it "syncs when parent value changes"
      (vui-defcomponent typed-sync-test ()
        :state ((value 10))
        :render
        (vui-vstack
          (vui-typed-field :type 'integer
                           :value value
                           :key 'test-field
                           :on-change (lambda (v) (vui-set-state :value v)))
          (vui-button "Set to 99"
            :key 'btn
            :on-click (lambda () (vui-set-state :value 99)))))
      (let ((instance (vui-mount (vui-component 'typed-sync-test)
                                  "*test-typed-sync*")))
        (unwind-protect
            (with-current-buffer "*test-typed-sync*"
              ;; Initial value should be 10
              (let ((widget (car widget-field-list)))
                (expect (widget-value widget) :to-equal "10"))
              ;; Click button to set value to 99
              (goto-char (point-min))
              (search-forward "[Set to 99]")
              (vui-components-test--click-button-at (- (point) 5))
              (vui-flush-sync)
              ;; The first flush processes the parent re-render; the child's
              ;; on-update detects the prop change and schedules another render
              (vui-flush-sync)
              ;; Field should now show 99
              (let ((widget (car widget-field-list)))
                (expect (widget-value widget) :to-equal "99")))
          (kill-buffer "*test-typed-sync*")))))

  (describe "error display"
    (it "shows error below field when show-error is t"
      (vui-defcomponent typed-show-error-test ()
        :render
        (vui-typed-field :type 'integer
                         :value 0
                         :key 'test-field
                         :show-error t
                         :on-change #'identity))
      (let ((instance (vui-mount (vui-component 'typed-show-error-test)
                                  "*test-typed-show-error*")))
        (unwind-protect
            (with-current-buffer "*test-typed-show-error*"
              (let ((widget (car widget-field-list)))
                ;; Type invalid input
                (widget-value-set widget "abc")
                (widget-apply widget :notify widget))
              (vui-flush-sync)
              ;; Error should be displayed in buffer
              (expect (buffer-string) :to-match "integer"))
          (kill-buffer "*test-typed-show-error*"))))

    (it "shows error inline when show-error is inline"
      (vui-defcomponent typed-show-error-inline-test ()
        :render
        (vui-typed-field :type 'integer
                         :value 0
                         :key 'test-field
                         :show-error 'inline
                         :on-change #'identity))
      (let ((instance (vui-mount (vui-component 'typed-show-error-inline-test)
                                  "*test-typed-show-error-inline*")))
        (unwind-protect
            (with-current-buffer "*test-typed-show-error-inline*"
              (let ((widget (car widget-field-list)))
                ;; Type invalid input
                (widget-value-set widget "abc")
                (widget-apply widget :notify widget))
              (vui-flush-sync)
              ;; Error should be displayed inline (on same line)
              (goto-char (point-min))
              (expect (looking-at ".*integer") :to-be-truthy))
          (kill-buffer "*test-typed-show-error-inline*"))))))

;; Note: These tests extract :type to a variable to work around an Emacs 29
;; byte-compilation issue where (plist-get props :type) returns nil
;; unexpectedly in buttercup `expect' forms when not pre-bound to a variable.
(describe "typed field shortcuts"
  (describe "vui-integer-field"
    (it "creates integer typed field component"
      (let* ((node (vui-integer-field :value 42))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type))
             (value-val (plist-get props :value)))
        (expect (vui-vnode-component-p node) :to-be-truthy)
        (expect type-val :to-equal 'integer)
        (expect value-val :to-equal 42)))

    (it "passes through all props"
      (let* ((node (vui-integer-field :value 10 :min 0 :max 100 :key 'age))
             (props (vui-vnode-component-props node))
             (min-val (plist-get props :min))
             (max-val (plist-get props :max))
             (key-val (plist-get props :key)))
        (expect min-val :to-equal 0)
        (expect max-val :to-equal 100)
        (expect key-val :to-equal 'age))))

  (describe "vui-natnum-field"
    (it "creates natnum typed field component"
      (let* ((node (vui-natnum-field :value 0))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type)))
        (expect type-val :to-equal 'natnum))))

  (describe "vui-float-field"
    (it "creates float typed field component"
      (let* ((node (vui-float-field :value 3.14))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type))
             (value-val (plist-get props :value)))
        (expect type-val :to-equal 'float)
        (expect value-val :to-equal 3.14))))

  (describe "vui-number-field"
    (it "creates number typed field component"
      (let* ((node (vui-number-field :value 42))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type)))
        (expect type-val :to-equal 'number))))

  (describe "vui-file-field"
    (it "creates file typed field component"
      (let* ((node (vui-file-field :value "/tmp/test.txt"))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type))
             (value-val (plist-get props :value)))
        (expect type-val :to-equal 'file)
        (expect value-val :to-equal "/tmp/test.txt"))))

  (describe "vui-directory-field"
    (it "creates directory typed field component"
      (let* ((node (vui-directory-field :value "/home/user"))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type)))
        (expect type-val :to-equal 'directory))))

  (describe "vui-symbol-field"
    (it "creates symbol typed field component"
      (let* ((node (vui-symbol-field :value 'my-symbol))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type))
             (value-val (plist-get props :value)))
        (expect type-val :to-equal 'symbol)
        (expect value-val :to-equal 'my-symbol))))

  (describe "vui-sexp-field"
    (it "creates sexp typed field component"
      (let* ((node (vui-sexp-field :value '(1 2 3)))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type))
             (value-val (plist-get props :value)))
        (expect type-val :to-equal 'sexp)
        (expect value-val :to-equal '(1 2 3))))

    (it "handles complex sexp"
      (let* ((node (vui-sexp-field :value '(:key "value" :num 42)))
             (props (vui-vnode-component-props node))
             (type-val (plist-get props :type)))
        (expect type-val :to-equal 'sexp)))))

;;; vui-components-test.el ends here

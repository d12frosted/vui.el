;;; vui-components-test.el --- Tests for vui-components.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui-components.el using Buttercup.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'vui-components)

;; Helper to click buttons (widget.el push-buttons)
(defun vui-components-test--click-button-at (pos)
  "Invoke the button widget at POS.
Buttons are widget.el push-buttons, so we use widget-apply."
  (let ((widget (widget-at pos)))
    (when widget
      (widget-apply widget :action))))

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
          (kill-buffer "*test-collapsible-16*"))))))

;;; vui-components-test.el ends here

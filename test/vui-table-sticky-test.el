;;; vui-table-sticky-test.el --- Sticky table header tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for `vui-table' :sticky-header (issue #117).
;;
;; Semantics under test: the header row renders into the buffer as
;; usual, and a pinned copy shows in `header-line-format' only while
;; the window is scrolled into the table's body - that is, when the
;; in-buffer header row is above `window-start' but the table has not
;; ended yet.  Multiple sticky tables hand the pinned header over as
;; the window moves from one into the next.
;;
;; `vui--table-sticky-header' takes an optional position standing in
;; for `window-start', which is what these tests drive.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-sticky--at (pos)
  "Pinned header at window-start POS, without the alignment prefix."
  (let ((s (vui--table-sticky-header pos)))
    (if (string-empty-p s) s (substring-no-properties s 1))))

(defun vui-sticky--pos-of (text)
  "Position of the line start of the first occurrence of TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (line-beginning-position)))

(describe "vui-table :sticky-header"
  (it "stores :sticky-header in the vnode"
    (let ((node (vui-table
                 :sticky-header t
                 :columns '((:header "A"))
                 :rows '(("1")))))
      (expect (vui-vnode-table-sticky-header node) :to-be-truthy))
    (let ((node (vui-table :columns '((:header "A")) :rows '(("1")))))
      (expect (vui-vnode-table-sticky-header node) :to-be nil)))

  (it "renders the buffer exactly like a non-sticky table"
    (let (plain sticky)
      (with-temp-buffer
        (vui-render (vui-table
                     :columns '((:header "Name" :width 6 :grow t)
                                (:header "Age" :width 4 :grow t))
                     :rows '(("Alice" "30"))
                     :border :ascii))
        (setq plain (buffer-string)))
      (with-temp-buffer
        (vui-render (vui-table
                     :sticky-header t
                     :columns '((:header "Name" :width 6 :grow t)
                                (:header "Age" :width 4 :grow t))
                     :rows '(("Alice" "30"))
                     :border :ascii))
        (setq sticky (buffer-string)))
      (expect sticky :to-equal plain)))

  (it "installs an :eval header line format"
    (with-temp-buffer
      (vui-render (vui-table
                   :sticky-header t
                   :columns '((:header "H"))
                   :rows '(("x"))))
      (expect header-line-format
              :to-equal '("" (:eval (vui--table-sticky-header))))))

  (it "pins the header only while the window is inside the table body"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "Title")
                   (vui-table
                    :sticky-header t
                    :columns '((:header "Name" :width 6 :grow t)
                               (:header "Age" :width 4 :grow t))
                    :rows '(("Alice" "30") ("Bob" "25")))
                   (vui-text "Footer")))
      (let ((header-pos (vui-sticky--pos-of "Name"))
            (row1-pos (vui-sticky--pos-of "Alice"))
            (row2-pos (vui-sticky--pos-of "Bob"))
            (footer-pos (vui-sticky--pos-of "Footer")))
        ;; Window at the top: the in-buffer header is visible, no pin
        (expect (vui-sticky--at (point-min)) :to-equal "")
        (expect (vui-sticky--at header-pos) :to-equal "")
        ;; Scrolled past the header row: pin appears
        (expect (vui-sticky--at row1-pos) :to-equal "Name   Age ")
        (expect (vui-sticky--at row2-pos) :to-equal "Name   Age ")
        ;; Past the table: pin goes away
        (expect (vui-sticky--at footer-pos) :to-equal ""))))

  (it "returns the header with border prefix and faces intact"
    (with-temp-buffer
      ;; Mock line-number-display-width
      (cl-letf (((symbol-function 'line-number-display-width)
                 (lambda (&optional type) (if (eq type 'columns) 4.0 28))))
        (vui-render (vui-table
                     :sticky-header t
                     :columns '((:header "A" :width 3 :grow t)
                                (:header "B" :width 3 :grow t))
                     :rows '(("1" "2"))
                     :border :ascii))
        (let* ((row-pos (vui-sticky--pos-of "| 1"))
               (pinned (vui--table-sticky-header row-pos)))
          (expect (substring-no-properties pinned 1) :to-equal "| A   | B   |")
          ;; Alignment prefix over the fringe, margin, and line numbers.
          (expect (get-text-property 0 'display pinned)
                  :to-equal '(space :align-to 4.0))
          ;; Faces come straight from the buffer text
          (let* ((plain (substring-no-properties pinned))
                 (cell-pos (cl-position ?A plain))
                 (border-pos (cl-position ?| plain)))
            (expect (get-text-property cell-pos 'face pinned)
                    :to-equal 'vui-table-header)
            (expect (get-text-property border-pos 'face pinned)
                    :to-equal 'vui-table-border))))))

  (it "hands the pin over between multiple sticky tables"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-table
                    :sticky-header t
                    :columns '((:header "First" :width 6 :grow t))
                    :rows '(("a1") ("a2")))
                   (vui-text "between")
                   (vui-table
                    :sticky-header t
                    :columns '((:header "Second" :width 6 :grow t))
                    :rows '(("b1") ("b2")))))
      (expect (vui-sticky--at (vui-sticky--pos-of "a1")) :to-equal "First ")
      (expect (vui-sticky--at (vui-sticky--pos-of "between")) :to-equal "")
      (expect (vui-sticky--at (vui-sticky--pos-of "Second")) :to-equal "")
      (expect (vui-sticky--at (vui-sticky--pos-of "b2")) :to-equal "Second")))

  (it "reads the header from the buffer, so width changes are live"
    (let ((vui-render-delay nil))
      (vui-defcomponent sticky-widths ()
        :state ((wide nil))
        :render (vui-table
                 :sticky-header t
                 :columns '((:header "C1") (:header "C2"))
                 :rows (list (list (if wide "wide-content" "aa") "b"))))
      (let ((inst (vui-mount (vui-component 'sticky-widths) "*sticky-w*")))
        (unwind-protect
            (with-current-buffer "*sticky-w*"
              (expect (vui-sticky--at (vui-sticky--pos-of "aa"))
                      :to-equal "C1 C2")
              (let ((vui--current-instance inst)) (vui-set-state :wide t))
              (expect (vui-sticky--at (vui-sticky--pos-of "wide-content"))
                      :to-equal (concat "C1" (make-string 10 ?\s) " C2")))
          (kill-buffer "*sticky-w*")))))

  (it "includes surrounding indentation, keeping columns aligned"
    (with-temp-buffer
      (vui-render (vui-vstack
                   :indent 2
                   (vui-table
                    :sticky-header t
                    :columns '((:header "H" :width 3 :grow t))
                    :rows '(("x")))))
      (expect (vui-sticky--at (vui-sticky--pos-of "x")) :to-equal "  H  ")))

  (it "escapes percent constructs in the pinned copy"
    (with-temp-buffer
      (vui-render (vui-table
                   :sticky-header t
                   :columns '((:header "Load %" :width 8 :grow t))
                   :rows '(("90%"))))
      (let ((pinned (vui--table-sticky-header (vui-sticky--pos-of "90%"))))
        (expect (substring-no-properties pinned) :to-match "Load %%"))))

  (it "stops pinning when a re-render drops the table"
    (let ((vui-render-delay nil))
      (vui-defcomponent sticky-cond ()
        :state ((show t))
        :render (if show
                    (vui-table
                     :sticky-header t
                     :columns '((:header "H" :width 3 :grow t))
                     :rows '(("x") ("y")))
                  (vui-text "gone")))
      (let ((inst (vui-mount (vui-component 'sticky-cond) "*sticky-c*")))
        (unwind-protect
            (with-current-buffer "*sticky-c*"
              (expect (vui-sticky--at (vui-sticky--pos-of "y"))
                      :to-equal "H  ")
              (let ((vui--current-instance inst)) (vui-set-state :show nil))
              ;; No table anywhere: every position pins nothing
              (expect (vui-sticky--at (point-min)) :to-equal "")
              (expect (vui-sticky--at (point-max)) :to-equal ""))
          (kill-buffer "*sticky-c*")))))

  (it "leaves rendering and header line alone when :sticky-header is nil"
    (with-temp-buffer
      (vui-render (vui-table
                   :columns '((:header "Name" :width 6 :grow t))
                   :rows '(("Alice"))))
      (expect (buffer-string) :to-equal "Name  \nAlice ")
      (expect header-line-format :to-be nil)))

  (it "does not touch the header line when no column has a header"
    (with-temp-buffer
      (vui-render (vui-table
                   :sticky-header t
                   :columns '((:width 3 :grow t))
                   :rows '(("x"))))
      (expect header-line-format :to-be nil)))

  (it "restores header-line-format on unmount"
    (vui-defcomponent sticky-unmount ()
      :render (vui-table
               :sticky-header t
               :columns '((:header "H"))
               :rows '(("x"))))
    (vui-mount (vui-component 'sticky-unmount) "*sticky-u*")
    (unwind-protect
        (with-current-buffer "*sticky-u*"
          (expect header-line-format :not :to-be nil)
          (vui-unmount)
          (expect header-line-format :to-be nil)
          (expect vui--table-saved-header-line :to-be nil)
          (expect vui--table-sticky-registry :to-be nil))
      (kill-buffer "*sticky-u*")))

  (it "restores the header line when remounting without a sticky table"
    (vui-defcomponent sticky-first ()
      :render (vui-table
               :sticky-header t
               :columns '((:header "H"))
               :rows '(("x"))))
    (vui-defcomponent plain-second ()
      :render (vui-text "no table"))
    (vui-mount (vui-component 'sticky-first) "*sticky-r*")
    (unwind-protect
        (progn
          (with-current-buffer "*sticky-r*"
            (expect header-line-format :not :to-be nil))
          (vui-mount (vui-component 'plain-second) "*sticky-r*")
          (with-current-buffer "*sticky-r*"
            (expect (buffer-string) :to-equal "no table")
            (expect header-line-format :to-be nil)))
      (kill-buffer "*sticky-r*")))

  (it "restores the header line when vui-render clears the buffer"
    (with-temp-buffer
      (vui-render (vui-table
                   :sticky-header t
                   :columns '((:header "H"))
                   :rows '(("x"))))
      (expect header-line-format :not :to-be nil)
      (vui-render (vui-text "plain"))
      (expect header-line-format :to-be nil)))

  (it "restores the header line when an inline instance unmounts"
    (with-temp-buffer
      (vui-defcomponent sticky-inline ()
        :render (vui-table
                 :sticky-header t
                 :columns '((:header "H"))
                 :rows '(("x"))))
      (insert "before\n")
      (let ((inst (vui-mount-inline (vui-component 'sticky-inline))))
        (expect header-line-format :not :to-be nil)
        (vui-unmount inst)
        (expect header-line-format :to-be nil))))

  (it "keeps the header line while another sticky table is still live"
    (with-temp-buffer
      (vui-defcomponent sticky-a ()
        :render (vui-table
                 :sticky-header t
                 :columns '((:header "A"))
                 :rows '(("a"))))
      (vui-defcomponent sticky-b ()
        :render (vui-table
                 :sticky-header t
                 :columns '((:header "B"))
                 :rows '(("b"))))
      (insert "host\n")
      (let ((ia (vui-mount-inline (vui-component 'sticky-a) (point)))
            (ib nil))
        (goto-char (point-max))
        (setq ib (vui-mount-inline (vui-component 'sticky-b) (point-max)))
        (expect header-line-format :not :to-be nil)
        ;; Unmounting one inline instance keeps the pin for the other
        (vui-unmount ia)
        (expect header-line-format :not :to-be nil)
        (vui-unmount ib)
        (expect header-line-format :to-be nil))))

  (it "errors when a sticky header cell is not a plain string"
    (with-temp-buffer
      (expect (vui-render (vui-table
                           :sticky-header t
                           :columns (list (list :header
                                                (vui-button "Sort"
                                                            :on-click #'ignore)))
                           :rows '(("x"))))
              :to-throw 'error)))

  (it "registers once through a measuring container"
    (with-temp-buffer
      (vui-render (vui-box
                   (vui-table
                    :sticky-header t
                    :columns '((:header "A" :width 3 :grow t))
                    :rows '(("1")))
                   :width 20))
      (expect (length vui--table-sticky-registry) :to-equal 1)
      (expect (vui-sticky--at (vui-sticky--pos-of "1")) :to-equal "A  "))))

(provide 'vui-table-sticky-test)
;;; vui-table-sticky-test.el ends here

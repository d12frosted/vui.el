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
  (it "pins a header row with a header over several columns"
    (with-temp-buffer
      (vui-render (vui-table
                   :sticky-header t
                   :columns '((:header "G" :header-span 2) () (:header "Z"))
                   :rows '(("a" "b" "c") ("d" "e" "f"))
                   :border :unicode))
      (expect (vui-sticky--at (1+ (vui-sticky--pos-of "│ d")))
              :to-equal "│ G     │ Z │")))

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

;;; The header line across a re-render

(defun vui-sticky--table (header)
  "Return a sticky table with one column titled HEADER and three rows."
  (vui-table :sticky-header t
             :columns (list (list :header header))
             :rows (list (list (concat (downcase header) "1"))
                         (list (concat (downcase header) "2"))
                         (list (concat (downcase header) "3")))))

(vui-defcomponent vui-sticky--switch (mode stream)
  "Text on top of what MODE asks for.
MODE is `table' (a sticky table), `both' (two sticky tables), `second'
\(text in place of the first table, then the second) or `text' (no
table).  With STREAM, a `vui-stream' handle, the content comes after
that stream instead."
  :render (let ((content
                 (pcase mode
                   ('table (list (vui-sticky--table "H")))
                   ('both (list (vui-sticky--table "H")
                                (vui-sticky--table "K")))
                   ('second (list (vui-text "gone")
                                  (vui-sticky--table "K")))
                   (_ (list (vui-text "plain"))))))
            (apply #'vui-vstack
                   (if stream (vui-stream stream) (vui-text "top"))
                   content)))

(vui-defcomponent vui-sticky--card (header table)
  "A sticky table titled HEADER when TABLE, else a line of text.
It renders again only when its props change."
  :memo t
  :render (if table
              (vui-sticky--table header)
            (vui-text (concat "no " header))))

(vui-defcomponent vui-sticky--cards (tables)
  "A card titled H and one titled K.  TABLES are the titles that show a table."
  :render (vui-vstack
           (vui-component 'vui-sticky--card :key "H" :header "H"
                          :table (and (member "H" tables) t))
           (vui-component 'vui-sticky--card :key "K" :header "K"
                          :table (and (member "K" tables) t))))

(vui-defcomponent vui-sticky--line (n)
  "A line of text showing N, with its newline."
  :render (vui-text (format "line %d\n" n)))

(vui-defcomponent vui-sticky--after-line (n table)
  "A `vui-sticky--line' showing N, then a card titled H showing TABLE.
They are in a fragment, so the card starts right where the line ends."
  :render (vui-fragment
           (vui-component 'vui-sticky--line :n n)
           (vui-component 'vui-sticky--card :header "H" :table table)))

(vui-defcomponent vui-sticky--after-stream (stream table)
  "STREAM, then a sticky table titled T when TABLE, else a line of text.
They are in a fragment, so the table starts right where the stream ends."
  :render (vui-fragment (vui-stream stream)
                        (if table
                            (vui-sticky--table "T")
                          (vui-text "plain"))))

(defmacro vui-sticky--with-switch (inst props &rest body)
  "Mount `vui-sticky--switch' with PROPS, run BODY in its buffer, clean up.
INST is bound to the root instance.  Its buffer gets a header line of
its own, \"mine\", for a sticky table to replace."
  (declare (indent 2))
  `(let* ((vui-render-delay nil)
          (,inst (vui-mount (vui-component 'vui-sticky--switch ,@props)
                            "*sticky-switch*")))
     (unwind-protect
         (with-current-buffer "*sticky-switch*"
           (setq header-line-format "mine")
           ,@body)
       (kill-buffer "*sticky-switch*"))))

(defun vui-sticky--mode (inst mode)
  "Re-render INST, a `vui-sticky--switch' root, with MODE."
  (vui-update inst (list :mode mode
                         :stream (plist-get (vui-instance-props inst)
                                            :stream))))

(describe "the sticky header line across a re-render"
  ;; A sticky table replaces the header line of its buffer.  When a
  ;; re-render leaves no sticky table, the header line it replaced comes
  ;; back, as on unmount.  With `vui-incremental-render' on, the list of
  ;; components is patched instead of rebuilt.  The other roots here are
  ;; rebuilt with the flag on too (a table is not text, so they cannot be
  ;; patched), an inline root always writes its region again, and the
  ;; content after a live stream is patched with the flag off as well.
  (dolist (flag '(nil t))
    (describe (format "with `vui-incremental-render' %s" flag)
      (it "puts back the header line when the table gives way to text"
        (let ((vui-incremental-render flag))
          (vui-sticky--with-switch inst (:mode 'text)
            (vui-sticky--mode inst 'table)
            (expect header-line-format :not :to-equal "mine")
            (vui-sticky--mode inst 'text)
            (expect (buffer-string) :to-equal "top\nplain")
            (expect header-line-format :to-equal "mine")
            (expect vui--table-saved-header-line :to-be nil)
            (expect vui--table-sticky-registry :to-be nil))))

      (it "keeps the header line pinned when the table renders again"
        (let ((vui-incremental-render flag))
          (vui-sticky--with-switch inst (:mode 'text)
            (vui-sticky--mode inst 'table)
            (vui-sticky--mode inst 'table)
            (expect header-line-format :not :to-equal "mine")
            (expect (vui-sticky--at (vui-sticky--pos-of "h2"))
                    :to-equal "H "))))

      (it "keeps the header line while another sticky table is shown"
        (let ((vui-incremental-render flag))
          (vui-sticky--with-switch inst (:mode 'text)
            (vui-sticky--mode inst 'both)
            (vui-sticky--mode inst 'second)
            (expect header-line-format :not :to-equal "mine")
            (expect (vui-sticky--at (vui-sticky--pos-of "k2"))
                    :to-equal "K ")
            (vui-sticky--mode inst 'text)
            (expect header-line-format :to-equal "mine"))))

      (it "puts back the header line when the tables of a list of components go"
        ;; With the flag on, the re-render writes again only the card
        ;; whose props changed, and leaves the text of the other alone
        (let ((vui-incremental-render flag)
              (vui-render-delay nil))
          (let ((inst (vui-mount (vui-component 'vui-sticky--cards :tables nil)
                                 "*sticky-cards*")))
            (unwind-protect
                (with-current-buffer "*sticky-cards*"
                  (vui-rerender inst)
                  (setq header-line-format "mine")
                  (vui-update inst '(:tables ("H" "K")))
                  (expect header-line-format :not :to-equal "mine")
                  (vui-update inst '(:tables ("K")))
                  (expect (buffer-string) :to-match "\\`no H\n")
                  (expect header-line-format :not :to-equal "mine")
                  (expect (vui-sticky--at (vui-sticky--pos-of "k2"))
                          :to-equal "K ")
                  (vui-update inst '(:tables nil))
                  (expect (buffer-string) :to-equal "no H\nno K")
                  (expect header-line-format :to-equal "mine"))
              (kill-buffer "*sticky-cards*")))))

      (it "puts back the header line when the table after a stream goes"
        ;; The re-render keeps the region of the live stream and renders
        ;; again only what comes after it
        (let ((vui-incremental-render flag)
              (stream (vui-make-stream)))
          (vui-sticky--with-switch inst (:mode 'text :stream stream)
            (vui-stream-append stream (vui-text "m0"))
            (vui-stream-append stream (vui-text "m1"))
            (vui-sticky--mode inst 'table)
            (expect header-line-format :not :to-equal "mine")
            (vui-sticky--mode inst 'text)
            (expect (buffer-string) :to-equal "m0\nm1\nplain")
            (expect header-line-format :to-equal "mine"))))

      (it "puts back the header line when a stream row drops its table"
        ;; A component row renders again on its own, not with the root
        (let ((vui-incremental-render flag)
              (stream (vui-make-stream)))
          (vui-sticky--with-switch inst (:mode 'text :stream stream)
            (vui-stream-append stream (vui-text "m0"))
            (vui-stream-append stream (vui-component 'vui-sticky--card
                                                     :header "R" :table t))
            (expect header-line-format :not :to-equal "mine")
            (vui-update (car (vui-stream-handle-items-rev stream))
                        '(:header "R" :table nil))
            (expect (buffer-string) :to-equal "m0\nno R\nplain")
            (expect header-line-format :to-equal "mine"))))

      (it "puts back the header line when an inline table gives way to text"
        (let ((vui-incremental-render flag)
              (vui-render-delay nil))
          (with-temp-buffer
            (setq header-line-format "mine")
            (insert "host\n")
            (let ((inst (vui-mount-inline
                         (vui-component 'vui-sticky--switch :mode 'table))))
              (expect header-line-format :not :to-equal "mine")
              (vui-sticky--mode inst 'text)
              (expect header-line-format :to-equal "mine")
              (vui-unmount inst)))))

      (it "leaves no sticky header line behind when a mode added to it"
        ;; A mode that adds its own part in front of the header line
        ;; while the table is shown (which-func does) takes it out
        ;; later; the sticky part must not be left behind then, with
        ;; nothing to put back
        (let ((vui-incremental-render flag))
          (vui-sticky--with-switch inst (:mode 'text)
            (vui-sticky--mode inst 'table)
            (add-to-list 'header-line-format "crumb")
            (vui-sticky--mode inst 'text)
            (setq header-line-format (delete "crumb" header-line-format))
            (expect header-line-format :to-equal "mine")
            (expect vui--table-saved-header-line :to-be nil))))

      (it "pins its own header after the line before it is written again"
        ;; With the flag on, only the line renders again: its new text is
        ;; inserted right where the table starts
        (let ((vui-incremental-render flag)
              (vui-render-delay nil))
          (let ((inst (vui-mount (vui-component 'vui-sticky--after-line
                                                :n 1 :table nil)
                                 "*sticky-after*")))
            (unwind-protect
                (with-current-buffer "*sticky-after*"
                  (vui-rerender inst)
                  (setq header-line-format "mine")
                  (vui-update inst '(:n 1 :table t))
                  (vui-update inst '(:n 22 :table t))
                  (expect (buffer-string) :to-match "\\`line 22\nH ")
                  (expect (vui-sticky--at (vui-sticky--pos-of "h2"))
                          :to-equal "H ")
                  (vui-update inst '(:n 22 :table nil))
                  (expect header-line-format :to-equal "mine"))
              (kill-buffer "*sticky-after*")))))

      (it "pins its own header after the stream before it changes"
        ;; Stream edits write at the end of the stream, which is where
        ;; the table starts
        (let ((vui-incremental-render flag)
              (vui-render-delay nil)
              (stream (vui-make-stream)))
          (let ((inst (vui-mount (vui-component 'vui-sticky--after-stream
                                                :stream stream :table nil)
                                 "*sticky-after*")))
            (unwind-protect
                (with-current-buffer "*sticky-after*"
                  (setq header-line-format "mine")
                  (vui-stream-append stream (vui-text "m0\n"))
                  (vui-update inst (list :stream stream :table t))
                  (vui-stream-append stream (vui-text "m1\n"))
                  (vui-stream-update-last stream (vui-text "m1!\n"))
                  (expect (buffer-string) :to-match "\\`m0\n\nm1!\nT ")
                  (expect (vui-sticky--at (vui-sticky--pos-of "t2"))
                          :to-equal "T ")
                  (vui-update inst (list :stream stream :table nil))
                  (expect header-line-format :to-equal "mine"))
              (kill-buffer "*sticky-after*")))))

      (it "pins its own header after the inline mount before it renders again"
        (let ((vui-incremental-render flag)
              (vui-render-delay nil))
          (with-temp-buffer
            (setq header-line-format "mine")
            (insert "host\n")
            (let* ((line (vui-mount-inline
                          (vui-component 'vui-sticky--line :n 1) (point-max)))
                   (card (vui-mount-inline
                          (vui-component 'vui-sticky--card :header "H" :table t)
                          (point-max))))
              (vui-update line '(:n 333))
              (expect (buffer-string) :to-match "\\`host\nline 333\nH ")
              (expect (vui-sticky--at (vui-sticky--pos-of "h2"))
                      :to-equal "H ")
              (vui-update card '(:header "H" :table nil))
              (expect header-line-format :to-equal "mine")
              (vui-unmount line)
              (vui-unmount card)))))))

  (it "puts back the header line before it restores the window start"
    ;; Text one line taller than the window body under the sticky header
    ;; line fits once that header line is gone, so the window shows it
    ;; from the top.  Restored after the window, it would not.
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-sticky--tall (lines)
        :render (if lines
                    (apply #'vui-vstack
                           (mapcar (lambda (i) (vui-text (format "t%d" i)))
                                   (number-sequence 1 lines)))
                  (vui-table :sticky-header t
                             :columns '((:header "H"))
                             :rows (mapcar (lambda (i) (list (format "r%d" i)))
                                           (number-sequence 1 60)))))
      (let ((inst (vui-mount (vui-component 'vui-sticky--tall)
                             "*sticky-tall*")))
        (unwind-protect
            (save-window-excursion
              (let ((win (selected-window)))
                (set-window-buffer win "*sticky-tall*")
                (with-current-buffer "*sticky-tall*"
                  (let ((height (window-body-height win)))
                    (set-window-start win (save-excursion
                                            (goto-char (point-min))
                                            (forward-line 3)
                                            (point)))
                    (goto-char (window-start win))
                    (forward-line 1)
                    (vui-update inst (list :lines (1+ height)))
                    (expect header-line-format :to-be nil)
                    (expect (window-start win) :to-equal (point-min))))))
          (kill-buffer "*sticky-tall*"))))))

(describe "the sticky header line across a stream edit"
  ;; An edit of a content item rewrites that item in place, without a
  ;; render of the root, so it has to put back the header line itself
  ;; when the item had the last sticky table.  The edits run from another
  ;; buffer, as from a process filter or a timer.
  (it "puts back the header line when update-last replaces a table item"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'text :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (vui-stream-append stream (vui-sticky--table "S"))
        (expect header-line-format :not :to-equal "mine")
        (with-temp-buffer (vui-stream-update-last stream (vui-text "m1")))
        (expect (buffer-string) :to-equal "m0\nm1\nplain")
        (expect header-line-format :to-equal "mine")
        (expect vui--table-saved-header-line :to-be nil)
        (expect vui--table-sticky-registry :to-be nil))))

  (it "keeps the header line pinned when update-last draws the table again"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'text :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (vui-stream-append stream (vui-sticky--table "S"))
        (vui-stream-update-last stream (vui-sticky--table "S"))
        (expect header-line-format :not :to-equal "mine")
        (expect (vui-sticky--at (vui-sticky--pos-of "s2")) :to-equal "S "))))

  (it "puts back the header line when a node drops its table"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'text :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (let ((node (vui-stream-open stream (vui-sticky--table "S"))))
          (vui-stream-append stream (vui-text "m2"))
          (expect header-line-format :not :to-equal "mine")
          (with-temp-buffer (vui-stream-update node (vui-text "m1")))
          (expect (buffer-string) :to-equal "m0\nm1\nm2\nplain")
          (expect header-line-format :to-equal "mine")))))

  (it "puts back the header line when the node with the table is removed"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'text :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (let ((node (vui-stream-open stream (vui-sticky--table "S"))))
          (vui-stream-append stream (vui-text "m2"))
          (expect header-line-format :not :to-equal "mine")
          (with-temp-buffer (vui-stream-remove node))
          (expect (buffer-string) :to-equal "m0\nm2\nplain")
          (expect header-line-format :to-equal "mine")))))

  (it "keeps the header line while the root still shows a sticky table"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'table :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (vui-stream-append stream (vui-sticky--table "S"))
        (vui-stream-update-last stream (vui-text "m1"))
        (expect header-line-format :not :to-equal "mine")
        (expect (vui-sticky--at (vui-sticky--pos-of "h2")) :to-equal "H "))))

  (it "keeps the header line when node edits leave the root's sticky table"
    (let ((stream (vui-make-stream)))
      (vui-sticky--with-switch inst (:mode 'table :stream stream)
        (vui-stream-append stream (vui-text "m0"))
        (let ((node (vui-stream-open stream (vui-sticky--table "S")))
              (other (vui-stream-open stream (vui-sticky--table "T"))))
          (vui-stream-update node (vui-text "m1"))
          (vui-stream-remove other)
          (expect (buffer-string) :to-match "\\`m0\nm1\nH ")
          (expect header-line-format :not :to-equal "mine")
          (expect (vui-sticky--at (vui-sticky--pos-of "h2")) :to-equal "H "))))))

(provide 'vui-table-sticky-test)
;;; vui-table-sticky-test.el ends here

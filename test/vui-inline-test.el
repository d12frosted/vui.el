;;; vui-inline-test.el --- Inline mounting tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for inline mounting (issue #8): vui-mount-inline,
;; region-scoped re-rendering inside host buffers, vui-unmount on
;; inline instances, and teardown integration.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)
(require 'vui-test-helpers)

;; Helper to click buttons (button.el text buttons)
(defun vui-inline-test--click-button-at (pos)
  "Invoke the text button at POS."
  (let ((button (button-at pos)))
    (when button
      (button-activate button))))

(defun vui-inline-test--button-pos (label)
  "Return the start of the button labelled LABEL in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "[%s]" label))
    (match-beginning 0)))

(defmacro vui-inline-test--with-side-window (window &rest body)
  "Run BODY with the current buffer shown in the non-selected WINDOW.
The selected window shows another buffer, so the current buffer's point
and WINDOW's point are two separate positions."
  (declare (indent 1))
  (let ((main (make-symbol "main"))
        (host (make-symbol "host")))
    `(let ((,host (current-buffer)))
       (save-window-excursion
         (let* ((,main (selected-window))
                (,window (split-window ,main)))
           (set-window-buffer ,main (get-buffer-create "*scratch*"))
           (set-window-buffer ,window ,host)
           ,@body)))))

(vui-defcomponent inline-window-point (extra)
  "Two rows of buttons, with a third row on top of them when EXTRA."
  :render (vui-fragment
           (when extra (vui-button "extra"))
           (when extra (vui-newline))
           (vui-button "one") (vui-newline)
           (vui-button "two") (vui-newline)))

(defun vui-inline-test--type (instance string)
  "Type STRING at point, one character at a time.
Each character is followed by the render INSTANCE schedules for it."
  (dolist (char (string-to-list string))
    (insert (char-to-string char))
    (vui-test--wait-for-render instance)))

(vui-defcomponent inline-code ()
  "A field of three characters, with a note after it once it is full."
  :state ((text ""))
  :render (vui-fragment
           (vui-text "Code: ")
           (vui-field :key 'code :value text :size 3
                      :on-change (lambda (value) (vui-set-state :text value)))
           (when (>= (length text) 3) (vui-text " (3 of 3)"))))

(vui-defcomponent inline-edge (tall)
  "One row, or three when TALL."
  :render (vui-text (if tall "[a]\n[b]\n[c]\n" "[a]\n")))

(vui-defcomponent inline-box (text)
  "TEXT, and nothing at all when TEXT is empty."
  :render (vui-text (or text "")))

(vui-defcomponent inline-fragile-child (fail)
  "A row, or an error when FAIL."
  :render (if fail (error "Boom") (vui-text "[ok]\n")))

(vui-defcomponent inline-fragile (fail)
  "A row, then a child whose render signals when FAIL."
  :render (vui-fragment
           (vui-text "[row]\n")
           (vui-component 'inline-fragile-child :fail fail)))

(defun vui-inline-test--box (text position)
  "Mount an `inline-box' showing TEXT at POSITION."
  (vui-mount-inline (vui-component 'inline-box :text text) position))

(defun vui-inline-test--host-insert (position text)
  "Insert TEXT at POSITION as a program that owns the host buffer does."
  (save-excursion
    (goto-char position)
    (insert text)))

(defun vui-inline-test--host-replace (start end text)
  "Replace the text from START to END with TEXT, as the host does.
The text can be that of a region, which is read-only outside of its
fields."
  (let ((inhibit-read-only t))
    (delete-region start end))
  (vui-inline-test--host-insert start text))

(defun vui-inline-test--bounds (instance)
  "Return (START . END), where the region markers of INSTANCE are."
  (cons (marker-position (vui-instance-region-start instance))
        (marker-position (vui-instance-region-end instance))))

(describe "vui-mount-inline"
  (it "renders at point without taking over the buffer"
    (with-temp-buffer
      (text-mode)
      (insert "HEAD\nTAIL")
      (goto-char 6)                     ; before "TAIL"
      (vui-defcomponent inline-hello ()
        :render (vui-fragment (vui-text "[form]") (vui-newline)))
      (let ((instance (vui-mount-inline (vui-component 'inline-hello))))
        (expect (vui-instance-p instance) :to-be-truthy)
        ;; Host mode and surrounding content untouched
        (expect major-mode :to-be 'text-mode)
        (expect (buffer-string) :to-equal "HEAD\n[form]\nTAIL"))))

  (it "mounts at an explicit position"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (goto-char (point-max))           ; point elsewhere
      (vui-defcomponent inline-positioned ()
        :render (vui-text "<X>"))
      (vui-mount-inline (vui-component 'inline-positioned) 6)
      (expect (buffer-string) :to-equal "HEAD\n<X>TAIL")))

  (it "leaves point before a component mounted at it"
    ;; A form opened at point has the cursor on its first row.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (goto-char 6)
      (vui-mount-inline (vui-component 'inline-window-point :extra nil))
      (expect (buffer-string) :to-equal "HEAD\n[one]\n[two]\nTAIL")
      (expect (point) :to-equal 6)))

  (it "leaves a window point before a component mounted at it"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-inline-test--with-side-window side
        (set-window-point side 6)
        (vui-mount-inline (vui-component 'inline-window-point :extra nil) 6)
        (expect (buffer-string) :to-equal "HEAD\n[one]\n[two]\nTAIL")
        (expect (window-point side) :to-equal 6))))

  (it "runs on-mount and effects"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-max))
      (let ((mounted nil)
            (effect-ran nil))
        (vui-defcomponent inline-lifecycle ()
          :on-mount (setq mounted t)
          :render (progn
                    (vui-use-effect ()
                      (setq effect-ran t)
                      nil)
                    (vui-text "x")))
        (vui-mount-inline (vui-component 'inline-lifecycle))
        (expect mounted :to-be t)
        (expect effect-ran :to-be t))))

  (it "rejects positions inside another inline instance"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-min))
      (vui-defcomponent inline-host-a ()
        :render (vui-text "[aaaa]"))
      (vui-mount-inline (vui-component 'inline-host-a))
      ;; Position 3 is strictly inside the [aaaa] region
      (expect (vui-mount-inline (vui-component 'inline-host-a) 3)
              :to-throw 'error)))

  (it "finds instances by position with vui-inline-instance-at"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-findable ()
        :render (vui-text "[form]"))
      (let ((instance (vui-mount-inline (vui-component 'inline-findable) 6)))
        ;; Inside the region (positions 6..12 cover "[form]")
        (expect (vui-inline-instance-at 8) :to-be instance)
        ;; Outside the region
        (expect (vui-inline-instance-at 2) :to-be nil)
        (expect (vui-inline-instance-at (point-max)) :to-be nil))))

  (it "finds an empty instance at its position"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "" 6)))
        (expect (vui-inline-instance-at 6) :to-be instance)
        ;; Text written there is the host's, the region is still at 6
        (vui-inline-test--host-insert 6 "host")
        (expect (vui-inline-instance-at 6) :to-be instance)
        (expect (vui-inline-instance-at 8) :to-be nil))))

  (it "finds the instance that starts at a position two regions share"
    ;; Whatever the order they were mounted in
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((b (vui-inline-test--box "[B]" 6))
             (a (vui-inline-test--box "[A]" 6)))
        (expect (buffer-string) :to-equal "HEAD\n[A][B]TAIL")
        (expect (vui-inline-instance-at 8) :to-be a)
        (expect (vui-inline-instance-at 9) :to-be b)))
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((a (vui-inline-test--box "[A]" 6))
             (b (vui-inline-test--box "[B]" 9)))
        (expect (buffer-string) :to-equal "HEAD\n[A][B]TAIL")
        (expect (vui-inline-instance-at 8) :to-be a)
        (expect (vui-inline-instance-at 9) :to-be b)))))

(describe "inline re-rendering"
  (it "re-renders only its region on state updates"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "HEAD\nTAIL")
        (goto-char 6)
        (vui-defcomponent inline-counter ()
          :state ((n 0))
          :render (vui-fragment
                   (vui-button (format "n=%d" n)
                     :on-click (lambda () (vui-set-state :n (1+ n))))
                   (vui-newline)))
        (vui-mount-inline (vui-component 'inline-counter))
        (expect (buffer-string) :to-equal "HEAD\n[n=0]\nTAIL")
        (vui-inline-test--click-button-at 6)
        (expect (buffer-string) :to-equal "HEAD\n[n=1]\nTAIL"))))

  (it "supports multiple independent instances in one buffer"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "ONE\nTWO\n")
        (vui-defcomponent inline-multi (tag)
          :state ((n 0))
          :render (vui-fragment
                   (vui-button (format "%s=%d" tag n)
                     :on-click (lambda () (vui-set-state :n (1+ n))))
                   (vui-newline)))
        ;; Mount bottom-most first so positions stay valid
        (vui-mount-inline (vui-component 'inline-multi :tag "b") 9)
        (vui-mount-inline (vui-component 'inline-multi :tag "a") 1)
        (expect (buffer-string) :to-equal "[a=0]\nONE\nTWO\n[b=0]\n")
        ;; Click the first form's button
        (vui-inline-test--click-button-at 1)
        (expect (buffer-string) :to-equal "[a=1]\nONE\nTWO\n[b=0]\n")
        ;; Click the second form's button ("[b=0]\n" is the last 6 chars)
        (vui-inline-test--click-button-at (- (point-max) 6))
        (expect (buffer-string) :to-equal "[a=1]\nONE\nTWO\n[b=1]\n"))))

  (it "preserves point outside the region across re-renders"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-growing (wide)
        :render (vui-text (if wide "[wiiiiide]" "[w]")))
      (let ((instance (vui-mount-inline (vui-component 'inline-growing
                                                       :wide nil)
                                        6)))
        ;; Put point on the "A" of TAIL, after the region
        (goto-char (- (point-max) 3))
        (expect (char-after) :to-equal ?A)
        ;; Grow the region; point must stay on the same character
        (vui-update instance '(:wide t))
        (expect (char-after) :to-equal ?A))))

  (it "keeps point inside a field across re-renders"
    (with-temp-buffer
      (insert "HOST\n")
      (goto-char (point-max))
      (vui-defcomponent inline-field-form ()
        :render (vui-fragment
                 (vui-text "Name: ")
                 (vui-field :value "abc" :size 10)
                 (vui-newline)))
      (let ((instance (vui-mount-inline (vui-component 'inline-field-form))))
        ;; Move point into the field
        (let ((field (car widget-field-list)))
          (goto-char (1+ (widget-field-start field))))
        (vui-rerender instance)
        (expect (widget-field-at (point)) :to-be-truthy))))

  (it "keeps a non-selected window's point on its widget inside the region"
    ;; A window that is not selected keeps its point in a marker, and
    ;; rewriting the region drops every marker in it at the region's
    ;; start.  The window gets the recovery the buffer's point gets.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-point :extra nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[one]\n[two]\nTAIL")
        (vui-inline-test--with-side-window side
          (set-window-point side (+ 2 (vui-inline-test--button-pos "two")))
          (vui-update instance '(:extra t))
          (expect (buffer-string)
                  :to-equal "HEAD\n[extra]\n[one]\n[two]\nTAIL")
          (expect (window-point side)
                  :to-equal (+ 2 (vui-inline-test--button-pos "two")))))))

  (it "keeps the point a window remembers inside the region"
    ;; The host buffer is hidden, and the window that showed it keeps
    ;; its point in `window-prev-buffers'.  The buffer's own point moved
    ;; elsewhere since, so the window's point is recovered on its own.
    ;; A buffer whose name starts with a space is never remembered, so
    ;; not `with-temp-buffer'.  The lines after TAIL make it taller than
    ;; the window, so a start put back by the point would not be at the
    ;; top.
    (with-current-buffer (get-buffer-create "*inline-hid*")
      (let ((lines (make-string 40 ?\n)))
        (insert "HEAD\nTAIL" lines)
        (let ((host (current-buffer))
              (other (get-buffer-create "*inline-hid-other*"))
              (instance (vui-mount-inline
                         (vui-component 'inline-window-point :extra nil) 6)))
          (unwind-protect
              (save-window-excursion
                (switch-to-buffer host)
                (goto-char (+ 2 (vui-inline-test--button-pos "two")))
                (switch-to-buffer other)
                (with-current-buffer host
                  (goto-char (point-min))
                  (vui-update instance '(:extra t))
                  (expect (buffer-string)
                          :to-equal (concat "HEAD\n[extra]\n[one]\n[two]\nTAIL"
                                            lines)))
                (switch-to-buffer host)
                (expect (window-point)
                        :to-equal (+ 2 (vui-inline-test--button-pos "two")))
                ;; The start is left alone, as for a window showing the
                ;; buffer, and it is outside the region
                (expect (window-start) :to-equal 1))
            (kill-buffer host)
            (kill-buffer other))))))

  (it "keeps a window point that is also the buffer's point inside the region"
    ;; The state a window is in right after it was left: its point and
    ;; the buffer's point are one position.  The window follows the
    ;; buffer's point, so that one has to be restored first.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-point :extra nil) 6)))
        (vui-inline-test--with-side-window side
          (goto-char (+ 2 (vui-inline-test--button-pos "two")))
          (set-window-point side (point))
          (vui-update instance '(:extra t))
          (expect (point)
                  :to-equal (+ 2 (vui-inline-test--button-pos "two")))
          (expect (window-point side) :to-equal (point))))))

  (it "follows a widget at the very start of the region"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-point :extra nil) 6)))
        (vui-inline-test--with-side-window side
          (set-window-point side (vui-inline-test--button-pos "one"))
          (expect (window-point side) :to-equal 6)
          (vui-update instance '(:extra t))
          (expect (window-point side)
                  :to-equal (vui-inline-test--button-pos "one"))))))

  (it "recovers a window point inside its own region, not in another one"
    ;; Two regions of one component in one buffer: their widgets have
    ;; the same places in the tree and the same labels, so the search
    ;; has to stay between the region's bounds.  The first region still
    ;; has the row the second one loses.
    (with-temp-buffer
      (insert "HEAD\nMID\nTAIL")
      (vui-defcomponent inline-window-two (extra)
        :render (vui-fragment
                 (vui-button "one") (vui-newline)
                 (vui-button "two") (vui-newline)
                 (when extra (vui-button "three"))
                 (when extra (vui-newline))))
      (let ((second (vui-mount-inline
                     (vui-component 'inline-window-two :extra t) 10)))
        (vui-mount-inline (vui-component 'inline-window-two :extra t) 6)
        (expect (buffer-string)
                :to-equal (concat "HEAD\n[one]\n[two]\n[three]\n"
                                  "MID\n[one]\n[two]\n[three]\nTAIL"))
        (vui-inline-test--with-side-window side
          (set-window-point side (- (point-max) 12)) ; the second "[three]"
          (expect (buffer-substring (window-point side)
                                    (+ 7 (window-point side)))
                  :to-equal "[three]")
          (vui-update second '(:extra nil))
          (expect (buffer-string)
                  :to-equal (concat "HEAD\n[one]\n[two]\n[three]\n"
                                    "MID\n[one]\n[two]\nTAIL"))
          ;; On the row above it in the second region.
          (expect (window-point side) :to-equal (- (point-max) 10))
          (expect (buffer-substring (window-point side)
                                    (+ 5 (window-point side)))
                  :to-equal "[two]")))))

  (it "leaves a window point outside the region to the text around it"
    ;; Only points inside the rewritten region need recovering; one in
    ;; the host text moves with that text, and must not be re-placed by
    ;; line and column while the region changes height above it.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-window-outside (tall)
        :render (vui-text (if tall "[a]\n[b]\n[c]\n" "[a]\n")))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-outside :tall nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[a]\nTAIL")
        (vui-inline-test--with-side-window side
          (set-window-point side (- (point-max) 3)) ; the "A" of TAIL
          (vui-update instance '(:tall t))
          (expect (buffer-string) :to-equal "HEAD\n[a]\n[b]\n[c]\nTAIL")
          (expect (char-after (window-point side)) :to-equal ?A)))))

  (it "keeps a window point at the end of the region on the host text"
    ;; The position right after the region is host text.  It moves with
    ;; that text when the region grows, like any position after it.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-window-edge (tall)
        :render (vui-text (if tall "[a]\n[b]\n[c]\n" "[a]\n")))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-edge :tall nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[a]\nTAIL")
        (vui-inline-test--with-side-window side
          (set-window-point side (- (point-max) 4)) ; the "T" of TAIL
          (vui-update instance '(:tall t))
          (expect (buffer-string) :to-equal "HEAD\n[a]\n[b]\n[c]\nTAIL")
          (expect (char-after (window-point side)) :to-equal ?T)))))

  (it "keeps point at the end of the region on the host text"
    ;; The same position, this time as the buffer's point.  Its line
    ;; and column belong to a row of the region once the region grew.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-edge :tall nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[a]\nTAIL")
        (goto-char (- (point-max) 4))   ; the "T" of TAIL
        (vui-update instance '(:tall t))
        (expect (buffer-string) :to-equal "HEAD\n[a]\n[b]\n[c]\nTAIL")
        (expect (char-after) :to-equal ?T))))

  (it "keeps point at the end of a region that ends the buffer"
    ;; The position after the region is the same one with no host text
    ;; behind it.
    (with-temp-buffer
      (insert "HEAD\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-edge :tall nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[a]\n")
        (goto-char (point-max))
        (vui-update instance '(:tall t))
        (expect (buffer-string) :to-equal "HEAD\n[a]\n[b]\n[c]\n")
        (expect (point) :to-equal (point-max)))))

  (it "keeps a window point at the end of a region that ends the buffer"
    (with-temp-buffer
      (insert "HEAD\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-edge :tall nil) 6)))
        (vui-inline-test--with-side-window side
          (goto-char (point-min))
          (set-window-point side (point-max))
          (vui-update instance '(:tall t))
          (expect (buffer-string) :to-equal "HEAD\n[a]\n[b]\n[c]\n")
          (expect (window-point side) :to-equal (point-max))))))

  (it "keeps point right after a region in the middle of a line"
    (with-temp-buffer
      (insert "say  now")
      (vui-defcomponent inline-chip (label)
        :render (vui-button label))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-chip :label "ok") 5)))
        (expect (buffer-string) :to-equal "say [ok] now")
        (goto-char 9)                   ; right after the chip
        (vui-update instance '(:label "cancel"))
        (expect (buffer-string) :to-equal "say [cancel] now")
        (expect (buffer-substring (point) (point-max)) :to-equal " now"))))

  (it "keeps point in a full field that ends the region"
    ;; The end of a field is a place to type at.  In a full field it is
    ;; also where the region ends, and the render the last character
    ;; asked for may put text after the field.
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (let ((instance (vui-mount-inline (vui-component 'inline-code) 6)))
          (goto-char (widget-field-start (car widget-field-list)))
          (vui-inline-test--type instance "abc")
          (expect (buffer-string) :to-equal "HEAD\nCode: abc (3 of 3)\nTAIL")
          (expect (buffer-substring (point-min) (point))
                  :to-equal "HEAD\nCode: abc")
          ;; and the field can still be edited from there
          (delete-char -1)
          (expect (buffer-substring (point-min) (point))
                  :to-equal "HEAD\nCode: ab")))))

  (it "re-renders a region mounted right after host text with a field property"
    ;; A prompt, say.  The property can hold anything, also a symbol
    ;; that names a widget type, and none of it is a field of ours.
    (with-temp-buffer
      (insert (propertize "prompt> " 'field 'text) "\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-edge :tall nil) 9)))
        (expect (buffer-string) :to-equal "prompt> [a]\n\nTAIL")
        (goto-char 9)
        (vui-update instance '(:tall t))
        (expect (buffer-string) :to-equal "prompt> [a]\n[b]\n[c]\n\nTAIL")
        (expect (point) :to-equal 9))))

  (it "keeps point in a full field before host text with a field property"
    ;; The same property on the text after the region is not an element
    ;; that starts where the field ends.
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n" (propertize "$ " 'field 'output))
        (let ((instance (vui-mount-inline (vui-component 'inline-code) 6)))
          (goto-char (widget-field-start (car widget-field-list)))
          (vui-inline-test--type instance "abc")
          (expect (buffer-string) :to-equal "HEAD\nCode: abc (3 of 3)$ ")
          (expect (buffer-substring (point-min) (point))
                  :to-equal "HEAD\nCode: abc")))))

  (it "keeps point at the end of a field whose text the render makes longer"
    ;; The component completes what was typed.  A cursor that was at the
    ;; end of the field is at its end again, not where the end was.
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (vui-defcomponent inline-year ()
          :state ((text ""))
          :render (vui-fragment
                   (vui-text "Date: ")
                   (vui-field
                    :key 'date :value text :size 4
                    :on-change (lambda (value)
                                 (vui-set-state
                                  :text (if (= (length value) 4)
                                            (concat value "-")
                                          value))))
                   (vui-text " (yyyy-mm)")))
        (let ((instance (vui-mount-inline (vui-component 'inline-year) 6)))
          (goto-char (widget-field-start (car widget-field-list)))
          (vui-inline-test--type instance "2024")
          (expect (buffer-string) :to-equal "HEAD\nDate: 2024- (yyyy-mm)\nTAIL")
          (expect (buffer-substring (point-min) (point))
                  :to-equal "HEAD\nDate: 2024-")))))

  (it "sends a position recovered onto the line after the region to its start"
    ;; Column 0 of the first line the region lost is where the region
    ;; ends now: the start of the host line that moved up.
    (with-temp-buffer
      (insert "HEAD\none\ntwo")
      (vui-defcomponent inline-lost-row (open)
        :render (vui-text (if open "card\n  a\n  b\n" "card\n")))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-lost-row :open t) 6)))
        (expect (buffer-string) :to-equal "HEAD\ncard\n  a\n  b\none\ntwo")
        (vui-inline-test--with-side-window side
          ;; the buffer's point
          (goto-char 11)
          (expect (looking-at-p "  a") :to-be-truthy)
          (set-window-point side (point-max))
          (vui-update instance '(:open nil))
          (expect (buffer-string) :to-equal "HEAD\ncard\none\ntwo")
          (expect (point) :to-equal 6)
          ;; a window's point
          (vui-update instance '(:open t))
          (goto-char (point-max))
          (set-window-point side 11)
          (vui-update instance '(:open nil))
          (expect (window-point side) :to-equal 6)))))

  (it "keeps a recovered window point inside the region when the region shrinks"
    ;; A point on plain text is recovered by line and column, and after
    ;; the region lost lines that line belongs to the host buffer.  The
    ;; start of the region is the fallback then.
    (with-temp-buffer
      (insert "HEAD\none\ntwo\nthree\nfour\nfive")
      (vui-defcomponent inline-window-shrink (open)
        :render (vui-text (if open "card\n  a\n  b\n  c\n  d\n" "card\n")))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-window-shrink :open t) 6)))
        (expect (buffer-string)
                :to-equal "HEAD\ncard\n  a\n  b\n  c\n  d\none\ntwo\nthree\nfour\nfive")
        (vui-inline-test--with-side-window side
          (set-window-point side (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "  d")
                                   (point)))
          (vui-update instance '(:open nil))
          (expect (buffer-string)
                  :to-equal "HEAD\ncard\none\ntwo\nthree\nfour\nfive")
          (expect (window-point side) :to-equal 6)))))

  (it "keeps a recovered point inside the region when the region shrinks"
    (with-temp-buffer
      (insert "HEAD\none\ntwo\nthree\nfour\nfive")
      (vui-defcomponent inline-point-shrink (open)
        :render (vui-text (if open "card\n  a\n  b\n  c\n  d\n" "card\n")))
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-point-shrink :open t) 6)))
        (goto-char (point-min))
        (search-forward "  d")
        (vui-update instance '(:open nil))
        (expect (buffer-string)
                :to-equal "HEAD\ncard\none\ntwo\nthree\nfour\nfive")
        (expect (point) :to-equal 6))))

  (it "does not accumulate field bookkeeping across re-renders"
    (with-temp-buffer
      (insert "HOST\n")
      (goto-char (point-max))
      (vui-defcomponent inline-one-field ()
        :render (vui-field :value "x" :size 5 :key 'inline-f1))
      (let ((instance (vui-mount-inline (vui-component 'inline-one-field))))
        (vui-rerender instance)
        (vui-rerender instance)
        (vui-rerender instance)
        (expect (length widget-field-list) :to-equal 1)
        (expect (vui-field-value 'inline-f1) :to-equal "x")))))

(describe "empty inline regions"
  ;; A region that renders nothing has no text of its own to hold its
  ;; place: its two markers are at one position.  Whatever is written
  ;; there, the region stays one region, in the place it had.
  (it "stays before text inserted at its position"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "" 6)))
        (vui-inline-test--host-insert 6 "host\n")
        (expect (vui-inline-test--bounds instance) :to-equal '(6 . 6))
        (vui-update instance '(:text "[B]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[B]\nhost\nTAIL")
        (expect (vui-inline-test--bounds instance) :to-equal '(6 . 10)))))

  (it "is below a region mounted at its position"
    ;; As a region with text is: what is mounted where it starts goes
    ;; above it.
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((b (vui-inline-test--box "" 6))
             (a (vui-inline-test--box "[A]\n" 6)))
        (expect (buffer-string) :to-equal "HEAD\n[A]\nTAIL")
        (expect (vui-inline-test--bounds a) :to-equal '(6 . 10))
        (expect (vui-inline-test--bounds b) :to-equal '(10 . 10))
        (vui-update a '(:text "[A]\n[A2]\n"))
        (expect (vui-inline-test--bounds b) :to-equal '(15 . 15))
        (vui-update b '(:text "[B]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[A2]\n[B]\nTAIL"))))

  (it "keeps its place below a region that re-renders"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((a (vui-inline-test--box "[A]\n" 6))
             (b (vui-inline-test--box "" 10)))
        (vui-update a '(:text "[A]\n[A2]\n"))
        (expect (vui-inline-test--bounds b) :to-equal '(15 . 15))
        (vui-update a '(:text "[A3]\n"))
        (expect (vui-inline-test--bounds b) :to-equal '(11 . 11))
        (vui-update b '(:text "[B]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[A3]\n[B]\nTAIL"))))

  (it "keeps its place above a region that re-renders"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((a (vui-inline-test--box "[A]\n" 6))
             (b (vui-inline-test--box "" 6)))
        (vui-update a '(:text "[A]\n[A2]\n"))
        (expect (vui-inline-test--bounds b) :to-equal '(6 . 6))
        (vui-update b '(:text "[B]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[B]\n[A]\n[A2]\nTAIL"))))

  (it "keeps its place between two regions that re-render"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((c (vui-inline-test--box "[C]\n" 6))
             (m (vui-inline-test--box "[M]\n" 6))
             (a (vui-inline-test--box "[A]\n" 6)))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[M]\n[C]\nTAIL")
        (vui-update m '(:text ""))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[C]\nTAIL")
        (vui-update a '(:text "[A]\n[A2]\n"))
        (vui-update c '(:text "[C2]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[A2]\n[C2]\nTAIL")
        (expect (vui-inline-test--bounds m) :to-equal '(15 . 15))
        (vui-update m '(:text "[M2]\n"))
        (expect (buffer-string)
                :to-equal "HEAD\n[A]\n[A2]\n[M2]\n[C2]\nTAIL"))))

  (it "keeps the order of empty regions at one position"
    ;; Whichever of them gets its text first
    (dolist (order '((a b c) (c b a) (b a c) (b c a)))
      (with-temp-buffer
        (insert "HEAD\nTAIL")
        (let* ((c (vui-inline-test--box "" 6))
               (b (vui-inline-test--box "" 6))
               (a (vui-inline-test--box "" 6)))
          (dolist (name order)
            (pcase name
              ('a (vui-update a '(:text "[A]\n")))
              ('b (vui-update b '(:text "[B]\n")))
              ('c (vui-update c '(:text "[C]\n")))))
          (expect (buffer-string) :to-equal "HEAD\n[A]\n[B]\n[C]\nTAIL")))))

  (it "keeps the order of regions that became empty at one position"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      ;; Mounted top to bottom this time
      (let* ((a (vui-inline-test--box "[A]\n" 6))
             (b (vui-inline-test--box "[B]\n" 10)))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[B]\nTAIL")
        (vui-update a '(:text ""))
        (vui-update b '(:text ""))
        (expect (buffer-string) :to-equal "HEAD\nTAIL")
        (vui-update a '(:text "[A]\n"))
        (vui-update b '(:text "[B]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[A]\n[B]\nTAIL"))))

  (it "leaves text written where its own text was deleted to the host"
    ;; The host deleted the region's text, the region is empty now
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "[B]\n" 6)))
        (vui-inline-test--host-replace 6 10 "host\n")
        (vui-update instance '(:text "[B2]\n"))
        (expect (buffer-string) :to-equal "HEAD\n[B2]\nhost\nTAIL")
        (expect (vui-inline-test--bounds instance) :to-equal '(6 . 11)))))

  (it "is around what a failed render wrote"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-fragile :fail nil) 6)))
        (expect (buffer-string) :to-equal "HEAD\n[row]\n[ok]\nTAIL")
        (expect (vui-update instance '(:fail t)) :to-throw 'error)
        ;; The old text is gone, the region holds the part that was
        ;; written before the error
        (expect (buffer-string) :to-equal "HEAD\n[row]\nTAIL")
        (expect (vui-inline-test--bounds instance) :to-equal '(6 . 12))
        (vui-update instance '(:fail nil))
        (expect (buffer-string) :to-equal "HEAD\n[row]\n[ok]\nTAIL")
        (expect (vui-inline-test--bounds instance) :to-equal '(6 . 17))))))

(defun vui-inline-test--model-failure (regions)
  "Return what differs between the buffer and REGIONS, or nil.
REGIONS is a list of (INSTANCE . TEXT), top to bottom.  The buffer is
to hold the texts in that order between \"HEAD\\n\" and \"TAIL\", each
of them between the region markers of its instance."
  (let ((pos 6)
        (failure nil))
    (unless (equal (buffer-string)
                   (concat "HEAD\n" (mapconcat #'cdr regions "") "TAIL"))
      (setq failure (list :buffer (buffer-string))))
    (dolist (region regions)
      (let ((end (+ pos (length (cdr region)))))
        (unless (or failure
                    (equal (vui-inline-test--bounds (car region))
                           (cons pos end)))
          (setq failure (list :text (cdr region)
                              :bounds (vui-inline-test--bounds (car region))
                              :expected (cons pos end))))
        (setq pos end)))
    failure))

(describe "inline regions next to each other"
  ;; Seeded runs of random mounts, updates and unmounts on a stack of
  ;; regions, half of which render nothing, against a model: a list of
  ;; the regions with their text, top to bottom.
  (it "keep their order and their bounds, whatever they render"
    (let ((failure nil))
      (cl-block done
        (dotimes (seed 60)
          (random (format "vui-inline-regions-%d" seed))
          (with-temp-buffer
            (insert "HEAD\nTAIL")
            (let ((regions nil)
                  (trace nil)
                  (count 0))
              (dotimes (_ 40)
                (let ((op (random 10))
                      (text (pcase (random 4)
                              ((or 0 1) "")
                              (2 (format "[%d]\n" (cl-incf count)))
                              (_ (format "<%d>" (cl-incf count))))))
                  (cond
                   ;; Mount where region INDEX starts, or after them all.
                   ;; The empty regions right before it are at that
                   ;; position too: the new region goes above them.
                   ((or (null regions)
                        (and (< op 3) (< (length regions) 7)))
                    (let* ((index (random (1+ (length regions))))
                           (position
                            (+ 6 (apply #'+ (mapcar (lambda (region)
                                                      (length (cdr region)))
                                                    (seq-take regions index)))))
                           (instance (vui-inline-test--box text position)))
                      (push (list 'mount index text) trace)
                      (while (and (> index 0)
                                  (equal (cdr (nth (1- index) regions)) ""))
                        (setq index (1- index)))
                      (setq regions (append (seq-take regions index)
                                            (list (cons instance text))
                                            (nthcdr index regions)))))
                   ((< op 4)
                    (let ((region (nth (random (length regions)) regions)))
                      (push (list 'unmount (cdr region)) trace)
                      (vui-unmount (car region))
                      (setq regions (delq region regions))))
                   (t
                    (let ((region (nth (random (length regions)) regions)))
                      (push (list 'update (cdr region) text) trace)
                      (vui-update (car region) (list :text text))
                      (setcdr region text)))))
                (when-let* ((difference
                             (vui-inline-test--model-failure regions)))
                  (setq failure (append (list :seed seed
                                              :trace (reverse trace))
                                        difference))
                  (cl-return-from done)))))))
      (expect failure :to-be nil))))

(describe "inline unmounting"
  (it "vui-unmount removes the region and runs full teardown"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((cleanups nil))
        (vui-defcomponent inline-teardown ()
          :on-mount (lambda () (push 'mount-cleanup cleanups))
          :on-unmount (push 'unmounted cleanups)
          :render (progn
                    (vui-use-effect ()
                      (lambda () (push 'effect-cleanup cleanups)))
                    (vui-text "[form]")))
        (let ((instance (vui-mount-inline (vui-component 'inline-teardown) 6)))
          (expect (buffer-string) :to-equal "HEAD\n[form]TAIL")
          (expect (vui-unmount instance) :to-be-truthy)
          ;; Region removed, host intact
          (expect (buffer-string) :to-equal "HEAD\nTAIL")
          (expect cleanups :to-have-same-items-as
                  '(mount-cleanup effect-cleanup unmounted))
          ;; No longer registered
          (expect (vui-inline-instance-at 6) :to-be nil)
          ;; Unmounting again is a no-op
          (expect (vui-unmount instance) :to-be nil)))))

  (it "leaves text written where the region's own text was deleted"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "[B]\n" 6)))
        (vui-inline-test--host-replace 6 10 "host\n")
        (vui-unmount instance)
        (expect (buffer-string) :to-equal "HEAD\nhost\nTAIL"))))

  (it "removes all of the region from a hook of its own render"
    ;; The render is half done then: the new text is in the buffer and
    ;; so is the old one
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance nil))
        (vui-defcomponent inline-dismiss-self (dismiss)
          :on-update (when dismiss (vui-unmount instance))
          :render (vui-text (if dismiss "[bye]\n" "[form]\n")))
        (setq instance (vui-mount-inline
                        (vui-component 'inline-dismiss-self :dismiss nil) 6))
        (expect (buffer-string) :to-equal "HEAD\n[form]\nTAIL")
        (vui-update instance '(:dismiss t))
        (expect (buffer-string) :to-equal "HEAD\nTAIL")
        (expect (vui-inline-instance-at 6) :to-be nil))))

  (it "removes an empty region from a hook of the render above it"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-defcomponent inline-dismiss-other (text victim)
        :on-update (when victim (vui-unmount victim))
        :render (vui-text (or text "")))
      (let* ((b (vui-mount-inline
                 (vui-component 'inline-dismiss-other :text "") 6))
             (a (vui-mount-inline
                 (vui-component 'inline-dismiss-other :text "") 6)))
        (vui-update a (list :text "[A]\n" :victim b))
        (expect (buffer-string) :to-equal "HEAD\n[A]\nTAIL")
        (expect (vui-inline-test--bounds a) :to-equal '(6 . 10))
        (expect (vui-inline-instance-at 10) :to-be a))))

  (it "makes stale async callbacks no-ops after unmount"
    (with-temp-buffer
      (insert "HOST")
      (goto-char (point-max))
      (let ((body-ran nil))
        (vui-defcomponent inline-stale ()
          :state ((n 0))
          :render (vui-text (format "[%d]" n)))
        (let* ((instance (vui-mount-inline (vui-component 'inline-stale)))
               (callback (let ((vui--current-instance instance)
                               (vui--root-instance instance))
                           (vui-with-async-context
                            (setq body-ran t)
                            (vui-set-state :n 99)))))
          (vui-unmount instance)
          (funcall callback)
          (expect body-ran :to-be nil)
          (expect (buffer-string) :to-equal "HOST")))))

  (it "runs teardown when the host buffer is killed"
    (let ((unmounted nil))
      (vui-defcomponent inline-kill-test ()
        :on-unmount (setq unmounted t)
        :render (vui-text "[form]"))
      (with-current-buffer (get-buffer-create "*test-inline-kill*")
        (insert "HOST")
        (goto-char (point-max))
        (vui-mount-inline (vui-component 'inline-kill-test)))
      (kill-buffer "*test-inline-kill*")
      (expect unmounted :to-be t)))

  (it "vui-mount tears down inline instances in the same buffer"
    (let ((unmounted nil))
      (vui-defcomponent inline-victim ()
        :on-unmount (setq unmounted t)
        :render (vui-text "[inline]"))
      (vui-defcomponent full-app ()
        :render (vui-text "FULL"))
      (with-current-buffer (get-buffer-create "*test-inline-full*")
        (insert "HOST")
        (goto-char (point-max))
        (vui-mount-inline (vui-component 'inline-victim)))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'full-app) "*test-inline-full*")
            (expect unmounted :to-be t)
            (expect (with-current-buffer "*test-inline-full*" (buffer-string))
                    :to-equal "FULL")
            (expect (with-current-buffer "*test-inline-full*"
                      vui--inline-instances)
                    :to-be nil))
        (kill-buffer "*test-inline-full*")))))

(vui-defcomponent inline-hooked (label mount-hook update-hook)
  "LABEL in brackets.  Its hooks call MOUNT-HOOK and UPDATE-HOOK."
  :on-mount (when mount-hook (funcall mount-hook))
  :on-update (when update-hook (funcall update-hook))
  :render (vui-text (format "[%s]" label)))

(vui-defcomponent inline-hooked-child (hook)
  "The text (c).  Its `:on-mount' calls HOOK."
  :on-mount (when hook (funcall hook))
  :render (vui-text "(c)"))

(vui-defcomponent inline-hooked-parent (label hook)
  "LABEL in brackets, then a child that calls HOOK when it mounts."
  :render (vui-fragment
           (vui-text (format "[%s]" label))
           (when hook (vui-component 'inline-hooked-child :hook hook))))

(vui-defcomponent inline-hooked-lines (lines update-hook)
  "LINES as they are.  Its `:on-update' calls UPDATE-HOOK."
  :on-update (when update-hook (funcall update-hook))
  :render (vui-text lines))

(vui-defcomponent inline-point-mover ()
  "The text (c).  Its `:on-mount' moves point to the end of the buffer."
  :on-mount (goto-char (point-max))
  :render (vui-text "(c)"))

(vui-defcomponent inline-point-mover-parent (label mover)
  "LABEL in brackets, a `inline-point-mover' when MOVER, then a tail."
  :render (vui-vstack
           (vui-text (format "[%s]" label))
           (when mover (vui-component 'inline-point-mover))
           (vui-text "tail")))

(defun vui-inline-test--region-text (instance)
  "Return the text of the region of INSTANCE."
  (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
    (buffer-substring-no-properties start end)))

(vui-defcomponent inline-padded-box (label)
  "A box with left padding around two lines: LABEL in brackets, then \"two\"."
  :render (vui-box (vui-vstack (vui-text (format "[%s]" label))
                               (vui-text "two"))
                   :width 10 :padding-left 2))

(vui-defcomponent inline-hooked-text (text hook)
  "TEXT as it is.  Its `:on-mount' calls HOOK."
  :on-mount (when hook (funcall hook))
  :render (vui-text text))

(vui-defcomponent inline-hooked-box (text hook)
  "An `inline-hooked-text' in a box with left padding."
  :render (vui-box (vui-component 'inline-hooked-text :text text :hook hook)
                   :width 8 :padding-left 2))

(vui-defcomponent inline-hooked-indented-box (text hook)
  "An `inline-hooked-text' in a box, in an indented vstack."
  :render (vui-vstack
           :indent 2
           (vui-box (vui-component 'inline-hooked-text :text text :hook hook)
                    :width 6)))

(vui-defcomponent inline-hooked-empty-box (hook)
  "An empty `inline-hooked-text' in a box, between two lines of an
indented vstack."
  :render (vui-vstack
           :indent 2
           (vui-text "xx")
           (vui-box (vui-component 'inline-hooked-text :text "" :hook hook)
                    :width 0)
           (vui-text "yy")))

(vui-defcomponent inline-hooked-table (text hook)
  "An `inline-hooked-text' in the first row of a table, indented."
  :render (vui-vstack
           :indent 2
           (vui-table :columns '((:width 6))
                      :rows (list (list (vui-component 'inline-hooked-text
                                                       :text text :hook hook))
                                  '("r2")))))

(vui-defcomponent inline-host-editor (edit text)
  "TEXT, or nothing when TEXT is nil.  Its `:on-mount' writes a line at
the start of the buffer when EDIT is `insert', and deletes the first
line of the buffer when EDIT is `delete'."
  :on-mount (let ((inhibit-read-only t))
              (pcase edit
                ('insert (save-excursion
                           (goto-char (point-min))
                           (insert "N1\n")))
                ('delete (delete-region (point-min)
                                        (save-excursion
                                          (goto-char (point-min))
                                          (line-beginning-position 2))))))
  :render (and text (vui-text text)))

(defvar vui-inline-test--keymap (make-sparse-keymap)
  "The keymap `inline-faced-layout' puts on its layout.")

(vui-defcomponent inline-faced-layout (layout edit prop)
  "A LAYOUT around an `inline-host-editor', with the face `bold' when
PROP is face, or `vui-inline-test--keymap' when PROP is keymap."
  :render (let ((child (vui-component 'inline-host-editor
                                      :edit edit :text "x"))
                (face (and (eq prop 'face) 'bold))
                (keymap (and (eq prop 'keymap) vui-inline-test--keymap)))
            (pcase layout
              ('box (vui-box child :width 4 :face face :keymap keymap))
              ('vstack (vui-vstack :face face :keymap keymap child))
              ('hstack (vui-hstack :face face :keymap keymap child))
              ('flex (vui-flex :width 4 :face face :keymap keymap child))
              ('flex-wrap (vui-flex :width 4 :wrap t :face face :keymap keymap
                                    child))
              ('grid (vui-grid :width 4 :columns 1 :face face :keymap keymap
                               child))
              ('region (vui-region :face face :keymap keymap child)))))

(vui-defcomponent inline-gapped-layout (layout edit)
  "A LAYOUT of aa, an `inline-host-editor' with no text, and bb."
  :render (let ((children (list (vui-text "aa")
                                (vui-component 'inline-host-editor
                                               :edit edit)
                                (vui-text "bb"))))
            (pcase layout
              ('vstack (apply #'vui-vstack children))
              ('hstack (apply #'vui-hstack children))
              ('flex (apply #'vui-flex :width 5 children))
              ;; A wrapping flex drops a child that measures as nothing:
              ;; a grower with no width of its own is written all the same
              ('flex-wrap (vui-flex :width 6 :wrap t
                                    (vui-text "aa")
                                    (vui-flex-item
                                     :grow 1
                                     (lambda (_width)
                                       (vui-component 'inline-host-editor
                                                      :edit edit)))
                                    (vui-text "bb"))))))

(vui-defcomponent inline-sticky-table ()
  "A table with a sticky header, and an `inline-host-editor' in its row."
  :render (vui-table :columns '((:header "H1" :min-width 4))
                     :rows (list (list (vui-component 'inline-host-editor
                                                      :edit 'insert
                                                      :text "x")))
                     :sticky-header t))

(vui-defcomponent inline-sticky-stream-table ()
  "A sticky table: `vui-inline-test--stream' in its first row, and an
`inline-stream-appender' in its second."
  :render (vui-table :columns '((:header "H1" :min-width 8))
                     :rows (list (list (vui-stream vui-inline-test--stream))
                                 (list (vui-component 'inline-stream-appender)))
                     :sticky-header t))

(describe "inline layouts whose components edit the host buffer"
  ;; The hook of a component in the layout runs while the layout is
  ;; written: the layout has to find its own text again after that
  (it "pins the header row of a sticky table whose rows edit each other"
    ;; The edit is in the table, after its header row
    (let ((vui-inline-test--stream (vui-make-stream " ")))
      (vui-stream-append vui-inline-test--stream (vui-text "s1"))
      (with-temp-buffer
        (insert "HOST\n")
        (vui-mount-inline (vui-component 'inline-sticky-stream-table))
        (let ((header (caar vui--table-sticky-registry)))
          (expect (buffer-substring-no-properties
                   header (save-excursion (goto-char header)
                                          (line-end-position)))
                  :to-match "\\`H1")))))

  (it "pins the header row of a sticky table, not the line above it"
    (with-temp-buffer
      (insert "HOST\n")
      (vui-mount-inline (vui-component 'inline-sticky-table))
      (let ((header (caar vui--table-sticky-registry)))
        (expect (buffer-substring-no-properties
                 header (save-excursion (goto-char header) (line-end-position)))
                :to-match "\\`H1"))))

  (dolist (prop '(face keymap))
    (dolist (layout '(box vstack hstack flex flex-wrap grid region))
      (dolist (edit '(insert delete))
        (it (format "puts the %s of the %s on its own text only (%s above)"
                    prop layout edit)
          (with-temp-buffer
            (insert "HOST\n")
            (let ((instance (vui-mount-inline
                             (vui-component 'inline-faced-layout
                                            :layout layout :edit edit
                                            :prop prop)))
                  (value (if (eq prop 'face) 'bold vui-inline-test--keymap)))
              (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
                (expect (text-property-not-all (point-min) start prop nil)
                        :to-be nil)
                (expect (text-property-not-all start end prop value)
                        :to-be nil)
                ;; All of the layout's text, kept
                (expect (buffer-substring-no-properties start end)
                        :to-equal (if (memq layout '(box grid)) "x   " "x")))))))))

  (dolist (layout '(vstack hstack flex flex-wrap))
    (dolist (edit '(insert delete))
      (it (format "drops the gap before a child with no text in the %s (%s above)"
                  layout edit)
        (with-temp-buffer
          (insert "HOST\n")
          (let ((instance (vui-mount-inline
                           (vui-component 'inline-gapped-layout
                                          :layout layout :edit edit)))
                (expected (with-temp-buffer
                            (vui-render (vui-component 'inline-gapped-layout
                                                       :layout layout))
                            (buffer-string))))
            (expect (vui-inline-test--region-text instance)
                    :to-equal expected)))))))

(defvar vui-inline-test--stream nil
  "A stream for `inline-faced-stream-stack'.")

(vui-defcomponent inline-stream-appender ()
  "Its `:on-mount' appends s2 to `vui-inline-test--stream'."
  :on-mount (vui-stream-append vui-inline-test--stream (vui-text "s2"))
  :render (vui-text "c"))

(vui-defcomponent inline-faced-stream-stack ()
  "A bold stack: a line, `vui-inline-test--stream', an appender to it."
  :render (vui-vstack :face 'bold
                      (vui-text "top")
                      (vui-stream vui-inline-test--stream)
                      (vui-component 'inline-stream-appender)))

(vui-defcomponent inline-failing-editor ()
  "Nothing.  Its `:on-mount' writes a line at the start of the buffer and
signals."
  :on-mount (progn (vui-inline-test--host-insert (point-min) "N1\n")
                   (error "Boom"))
  :render (ignore))

(vui-defcomponent inline-bounded-stack ()
  "A stack with an `inline-failing-editor' in an error boundary that
shows nothing for the error."
  :render (vui-vstack (vui-text "aa")
                      (vui-error-boundary
                       :fallback (lambda (_error) nil)
                       :children (list (vui-component 'inline-failing-editor)))
                      (vui-text "bb")))

(vui-defcomponent inline-unmount-editor ()
  "Its `:on-unmount' writes a line at the start of the buffer."
  :on-unmount (vui-inline-test--host-insert (point-min) "N1\n")
  :render (vui-text "u"))

(vui-defcomponent inline-toggler (show)
  "An `inline-unmount-editor' while SHOW, and nothing after."
  :render (and show (vui-component 'inline-unmount-editor)))

(vui-defcomponent inline-toggled-stack (show)
  "A bold stack with an `inline-toggler' in it."
  :render (vui-vstack :face 'bold
                      (vui-text "aa")
                      (vui-component 'inline-toggler :show show)
                      (vui-text "bb")))

(vui-defcomponent inline-other-editor ()
  "Its `:on-mount' writes a line at the start of its own buffer."
  :on-mount (let ((inhibit-read-only t))
              (save-excursion (goto-char (point-min)) (insert "OTHER\n")))
  :render (vui-text "o"))

(vui-defcomponent inline-renders-other (text)
  "Mounts an `inline-other-editor' in another buffer from its render,
and shows TEXT, or nothing when TEXT is nil."
  :render (progn
            (save-window-excursion
              (vui-mount (vui-component 'inline-other-editor)
                         " *test-other-tree*"))
            (and text (vui-text text))))

(vui-defcomponent inline-other-stack (text)
  "A bold stack with an `inline-renders-other' showing TEXT."
  :render (vui-vstack :face 'bold
                      (vui-text "aa")
                      (vui-component 'inline-renders-other :text text)
                      (vui-text "bb")))

(defun vui-inline-test--faced-only (instance)
  "Return non-nil when the face of INSTANCE's region is on it alone."
  (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
    (and (null (text-property-not-all (point-min) start 'face nil))
         (null (text-property-not-all start end 'face 'bold))
         (null (text-property-not-all end (point-max) 'face nil)))))

(describe "inline layouts that are edited while they are written"
  (it "keeps the face on all of a stack whose component appends to a stream in it"
    ;; The edit is in the text the stack wrote before the component
    (let ((vui-inline-test--stream (vui-make-stream)))
      (vui-stream-append vui-inline-test--stream (vui-text "s1"))
      (with-temp-buffer
        (insert "HOST\n")
        (let ((instance (vui-mount-inline
                         (vui-component 'inline-faced-stream-stack))))
          (expect (vui-inline-test--region-text instance)
                  :to-equal "top\ns1\ns2\nc")
          (expect (vui-inline-test--faced-only instance) :to-be t)))))

  (it "drops the gap of a child whose hook edits above and signals"
    (let ((vui-lifecycle-error-handler 'signal))
      (with-temp-buffer
        (insert "HOST\n")
        (let ((instance (vui-mount-inline
                         (vui-component 'inline-bounded-stack))))
          (expect (buffer-substring-no-properties 1 9) :to-equal "N1\nHOST\n")
          (expect (vui-inline-test--region-text instance)
                  :to-equal "aa\nbb")))))

  (it "keeps the face on its own text when the hook of a removed component edits above"
    (with-temp-buffer
      (insert "HOST\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-toggled-stack :show t))))
        ;; The removed component's hook runs while the stack is written
        (vui-update instance '(:show nil))
        (expect (buffer-substring-no-properties 1 9) :to-equal "N1\nHOST\n")
        (expect (vui-inline-test--region-text instance) :to-equal "aa\nbb")
        (expect (vui-inline-test--faced-only instance) :to-be t))))

  (dolist (text '("x" nil))
    (it (format "is not moved by the hooks of a render in another buffer (%s)"
                (if text "a child with text" "a child with none"))
      (unwind-protect
          (with-temp-buffer
            (insert "HOST\n\nTAIL")
            (let ((instance (vui-mount-inline
                             (vui-component 'inline-other-stack :text text)
                             6)))
              (expect (buffer-substring-no-properties
                       (cdr (vui--inline-bounds instance)) (point-max))
                      :to-equal "\nTAIL")
              (expect (vui-inline-test--region-text instance)
                      :to-equal (if text "aa\nx\nbb" "aa\nbb"))
              (expect (vui-inline-test--faced-only instance) :to-be t)))
        (when (get-buffer " *test-other-tree*")
          (kill-buffer " *test-other-tree*"))))))

(describe "inline regions with indented layouts"
  (it "leaves the host lines after the region as they are"
    (with-temp-buffer
      (insert "host\nline1\nline2\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-padded-box :label "a") 6)))
        (expect (buffer-substring-no-properties
                 (cdr (vui--inline-bounds instance)) (point-max))
                :to-equal "line1\nline2\n")
        (vui-update instance '(:label "b"))
        (expect (buffer-substring-no-properties
                 (cdr (vui--inline-bounds instance)) (point-max))
                :to-equal "line1\nline2\n")
        ;; What the box renders in a buffer of its own
        (expect (vui-inline-test--region-text instance)
                :to-equal (with-temp-buffer
                            (vui-render (vui-component 'inline-padded-box
                                                       :label "b"))
                            (buffer-string))))))

  ;; The hook of a component in the layout runs while the layout is
  ;; written, before it adds its indent
  (pcase-dolist (`(,layout ,edit ,text ,name)
                 '((inline-hooked-box insert "a\nb"
                    "a box whose component writes host text above it")
                   (inline-hooked-box delete "a\nb\nc"
                    "a box whose component deletes host text above it")
                   (inline-hooked-box delete-more "x"
                    "a box whose component deletes more host text above it than it writes")
                   (inline-hooked-indented-box insert "a\nb"
                    "a box in a vstack whose component writes host text above it")
                   (inline-hooked-indented-box delete "a\nb\nc"
                    "a box in a vstack whose component deletes host text above it")
                   (inline-hooked-indented-box delete-more "x"
                    "a box in a vstack whose component deletes more host text above it than it writes")
                   (inline-hooked-table insert "x"
                    "a table whose component writes host text above it")
                   (inline-hooked-table delete-more "x"
                    "a table whose component deletes more host text above it than it writes")))
    (it (format "indents %s" name)
      (let ((host "ABCDEFGHIJ\n\nAFTER1\nAFTER2")
            (hook (pcase edit
                    ('insert (lambda ()
                               (vui-inline-test--host-insert 1 "N1\n")))
                    ;; Fewer characters than the layout writes, or more
                    ('delete (lambda ()
                               (let ((inhibit-read-only t))
                                 (delete-region 1 4))))
                    ('delete-more (lambda ()
                                    (let ((inhibit-read-only t))
                                      (delete-region 1 12))))))
            (expected nil))
        (with-temp-buffer
          (insert host)
          (setq expected (vui-inline-test--region-text
                          (vui-mount-inline
                           (vui-component layout :text text) 12))))
        (with-temp-buffer
          (insert host)
          (let ((instance (vui-mount-inline
                           (vui-component layout :text text :hook hook)
                           12)))
            (expect (vui-inline-test--region-text instance)
                    :to-equal expected)
            (expect (buffer-substring-no-properties
                     (cdr (vui--inline-bounds instance)) (point-max))
                    :to-equal "\nAFTER1\nAFTER2"))))))

  ;; The vstack drops the separator and the indent it wrote before a
  ;; child that shows nothing
  (pcase-dolist (`(,edit ,name)
                 '((insert "writes host text above it")
                   (delete "deletes host text above it")))
    (it (format "drops the line of an empty box whose component %s" name)
      (with-temp-buffer
        (insert "ABCDEFGHIJ\n\nAFTER")
        (let* ((hook (pcase edit
                       ('insert (lambda ()
                                  (vui-inline-test--host-insert 1 "N1\n")))
                       ('delete (lambda ()
                                  (let ((inhibit-read-only t))
                                    (delete-region 1 4))))))
               (instance (vui-mount-inline
                          (vui-component 'inline-hooked-empty-box :hook hook)
                          12)))
          (expect (vui-inline-test--region-text instance)
                  :to-equal "  xx\n  yy")
          (expect (buffer-substring-no-properties
                   (cdr (vui--inline-bounds instance)) (point-max))
                  :to-equal "\nAFTER"))))))

(describe "inline regions whose hooks edit the host buffer"
  ;; The hooks of a component run inside its render.  What they write or
  ;; delete outside the region belongs to the host.
  (it "keeps the host text a hook writes above the region"
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance
             (vui-mount-inline
              (vui-component 'inline-hooked
                             :label "a"
                             :mount-hook (lambda ()
                                           (vui-inline-test--host-insert
                                            (point-min) "note: "))))))
        (expect (buffer-string) :to-equal "note: host line\n[a]")
        (expect (vui-inline-test--region-text instance) :to-equal "[a]")
        (vui-update instance '(:label "b"))
        (expect (buffer-string) :to-equal "note: host line\n[b]"))))

  (it "keeps the host text a hook writes right after the region"
    ;; A region with text: what is written right after an empty one,
    ;; where the render writes too, goes into it
    (with-temp-buffer
      (insert "host\nend")
      (let ((instance
             (vui-mount-inline (vui-component 'inline-hooked :label "a") 6)))
        (vui-update instance
                    (list :label "a"
                          :update-hook (lambda ()
                                         (vui-inline-test--host-insert
                                          (- (point-max) 3) " (tail)"))))
        (expect (buffer-string) :to-equal "host\n[a] (tail)end")
        (expect (vui-inline-test--region-text instance) :to-equal "[a]")
        (vui-update instance '(:label "b"))
        (expect (buffer-string) :to-equal "host\n[b] (tail)end"))))

  (it "keeps the host text a hook writes above the region when it renders again"
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-hooked :label "a"))))
        (vui-update instance
                    (list :label "b"
                          :update-hook (lambda ()
                                         (vui-inline-test--host-insert
                                          (point-min) "note: "))))
        (expect (buffer-string) :to-equal "note: host line\n[b]")
        (vui-update instance '(:label "c"))
        (expect (buffer-string) :to-equal "note: host line\n[c]"))))

  (it "keeps its text when a hook deletes host text above the region"
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-hooked :label "a"))))
        (vui-update instance
                    (list :label "b"
                          :update-hook (lambda ()
                                         (let ((inhibit-read-only t))
                                           (delete-region 1 6)))))
        (expect (buffer-string) :to-equal "line\n[b]")
        (expect (vui-inline-test--region-text instance) :to-equal "[b]")
        (vui-update instance '(:label "c"))
        (expect (buffer-string) :to-equal "line\n[c]"))))

  (it "keeps point in the region when a hook writes above it"
    ;; Point is on plain text, which no widget finds again.  Its line
    ;; counts from the start of the region, so it stays on its text, and
    ;; not where that was before the hook
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-hooked-lines
                                      :lines "[a]\n[b]"))))
        (goto-char (1- (point-max)))
        (vui-update instance
                    (list :lines "[a]\n[b]"
                          :update-hook (lambda ()
                                         (vui-inline-test--host-insert
                                          (point-min) "n1\nn2\n"))))
        (expect (buffer-string) :to-equal "n1\nn2\nhost line\n[a]\n[b]")
        (expect (point) :to-equal (1- (point-max))))))

  (it "keeps a window's point in the region when a hook writes above it"
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance (vui-mount-inline
                       (vui-component 'inline-hooked-lines
                                      :lines "[a]\n[b]"))))
        (vui-inline-test--with-side-window side
          (set-window-point side (1- (point-max)))
          (vui-update instance
                      (list :lines "[a]\n[b]"
                            :update-hook (lambda ()
                                           (vui-inline-test--host-insert
                                            (point-min) "n1\nn2\n"))))
          (expect (window-point side) :to-equal (1- (point-max)))))))

  ;; The hooks of a child run while the tree is still being written
  (dolist (when-mounted '(first later))
    (it (format "writes the rest of the tree in place when a child's hook moves point (%s render)"
                when-mounted)
      (with-temp-buffer
        (insert "host\n")
        (let ((instance
               (vui-mount-inline
                (vui-component 'inline-point-mover-parent
                               :label "a" :mover (eq when-mounted 'first)))))
          (vui-update instance '(:label "b" :mover t))
          (expect (buffer-string) :to-equal "host\n[b]\n(c)\ntail")
          (expect (vui-inline-test--region-text instance)
                  :to-equal "[b]\n(c)\ntail")
          (vui-update instance '(:label "c" :mover t))
          (expect (buffer-string) :to-equal "host\n[c]\n(c)\ntail")))))

  (it "removes what it wrote when a child's hook unmounts it"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-mount-inline
       (vui-component 'inline-hooked-parent
                      :label "a"
                      :hook (lambda () (vui-unmount (vui-inline-instance-at 6))))
       6)
      (expect (buffer-string) :to-equal "HEAD\nTAIL")))

  (it "keeps the host text a child writes above the region"
    (with-temp-buffer
      (insert "host line\n")
      (let ((instance
             (vui-mount-inline
              (vui-component 'inline-hooked-parent
                             :label "a"
                             :hook (lambda ()
                                     (vui-inline-test--host-insert
                                      (point-min) "note: "))))))
        (expect (buffer-string) :to-equal "note: host line\n[a](c)")
        (expect (vui-inline-test--region-text instance) :to-equal "[a](c)")
        (vui-update instance (list :label "b"
                                   :hook (plist-get (vui-instance-props instance)
                                                    :hook)))
        (expect (buffer-string) :to-equal "note: host line\n[b](c)"))))

  (it "keeps the host text a child writes right after the region"
    (with-temp-buffer
      (insert "host\nend")
      (let* ((tail (lambda ()
                     (vui-inline-test--host-insert
                      (- (point-max) 3) " (tail)")))
             (instance
              (vui-mount-inline
               (vui-component 'inline-hooked-parent :label "a")
               6)))
        ;; The child mounts in this render, after the region has text
        (vui-update instance (list :label "a" :hook tail))
        (expect (buffer-string) :to-equal "host\n[a](c) (tail)end")
        (expect (vui-inline-test--region-text instance) :to-equal "[a](c)")
        (vui-update instance (list :label "b" :hook tail))
        (expect (buffer-string) :to-equal "host\n[b](c) (tail)end")))))

;; Issue #254: a mounted form must not make the host text read-only
(vui-defcomponent inline-host-form ()
  "A field and a button, as in a form that collects input."
  :state ((name ""))
  :render (vui-fragment
           (vui-text "Name: ")
           (vui-field :value name :size 4
                      :on-change (lambda (value) (vui-set-state :name value)))
           (vui-button "OK")
           (vui-newline)))

(defun vui-inline-test--host-hook (&rest _)
  "A change hook of the host buffer.")

(vui-defcomponent inline-host-log (log)
  "The stream LOG."
  :render (vui-stream log))

(describe "host edits around inline regions"
  (it "edits host text while a form is mounted"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (vui-mount-inline (vui-component 'inline-host-form) 6)
      (goto-char (point-min))
      (insert "> ")
      (goto-char (point-max))
      (insert "!")
      (delete-region (- (point-max) 2) (point-max))
      (expect (buffer-substring (point-min) 8) :to-equal "> HEAD\n")
      (expect (buffer-substring (- (point-max) 4) (point-max))
              :to-equal "\nTAI")))

  (it "types at the edges of a region, outside of it"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((instance (vui-mount-inline (vui-component 'inline-host-form) 6))
             (form (vui-inline-test--region-text instance)))
        (goto-char 6)
        (insert "a")
        (goto-char (cdr (vui-inline-test--bounds instance)))
        (insert "b")
        (expect (buffer-string) :to-equal (concat "HEAD\na" form "bTAIL"))
        (expect (vui-inline-test--region-text instance) :to-equal form))))

  (it "refuses edits of a region's text outside of its fields"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let* ((instance (vui-mount-inline (vui-component 'inline-host-form) 6))
             (form (vui-inline-test--region-text instance)))
        (goto-char 8)
        (expect (insert "x") :to-throw 'text-read-only)
        ;; The next command: the refusal emptied the change hooks
        (run-hooks 'post-command-hook)
        ;; Across the start of the region
        (expect (delete-region 4 8) :to-throw 'text-read-only)
        (expect (vui-inline-test--region-text instance) :to-equal form))))

  (it "types in a field of a region"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-mount-inline (vui-component 'inline-host-form) 6)))
        (goto-char (+ 6 (length "Name: ")))
        (vui-inline-test--type instance "ab")
        (expect (vui-inline-test--region-text instance)
                :to-match "\\`Name: ab"))))

  (it "puts back the change hooks after a refused edit"
    ;; Emacs empties `before-change-functions' when one of them signals
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (add-hook 'before-change-functions #'vui-inline-test--host-hook nil t)
      (vui-mount-inline (vui-component 'inline-host-form) 6)
      (let ((hooks before-change-functions))
        (goto-char 8)
        (ignore-errors (insert "x"))
        (run-hooks 'post-command-hook)
        (expect before-change-functions :to-equal hooks)
        (goto-char (point-min))
        (insert "> ")
        (expect (buffer-substring (point-min) 3) :to-equal "> "))))

  (it "leaves the host buffer as it was after the last form unmounts"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (add-hook 'before-change-functions #'vui-inline-test--host-hook nil t)
      (let ((before before-change-functions)
            (after after-change-functions)
            (a (vui-mount-inline (vui-component 'inline-host-form) 6))
            (b (vui-mount-inline (vui-component 'inline-host-form) 1)))
        (vui-unmount b)
        (goto-char 8)
        (expect (insert "x") :to-throw 'text-read-only)
        (vui-unmount a)
        (expect (buffer-string) :to-equal "HEAD\nTAIL")
        (expect before-change-functions :to-equal before)
        (expect after-change-functions :to-equal after)
        (goto-char 7)
        (insert "x")
        (expect (buffer-string) :to-equal "HEAD\nTxAIL"))))

  (it "leaves the host buffer as it was after a form with stream rows unmounts"
    ;; The rows go with the form, and their regions are no longer in the
    ;; buffer
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "a"))
      (with-temp-buffer
        (insert "HEAD\nTAIL")
        (let ((before before-change-functions)
              (after after-change-functions)
              (form (vui-mount-inline (vui-component 'inline-host-log :log log)
                                      6)))
          (vui-stream-append log (vui-component 'inline-box :text "[row]"))
          (vui-unmount form)
          (expect (buffer-string) :to-equal "HEAD\nTAIL")
          (expect before-change-functions :to-equal before)
          (expect after-change-functions :to-equal after)))))

  (it "deletes host text around an empty region"
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "" 6)))
        (delete-region 3 8)
        (expect (buffer-string) :to-equal "HEIL")
        (expect (vui-inline-test--bounds instance) :to-equal '(3 . 3)))))

  (it "keeps text inserted before markers at the end of a region outside"
    ;; As process output is inserted
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((instance (vui-inline-test--box "[box]" 6)))
        (goto-char 11)
        (insert-before-markers "OUT")
        ;; Process output binds `inhibit-read-only' too
        (let ((inhibit-read-only t))
          (goto-char 11)
          (insert-before-markers "!"))
        (expect (vui-inline-test--region-text instance) :to-equal "[box]")
        (vui-rerender instance)
        (expect (buffer-string) :to-equal "HEAD\n[box]!OUTTAIL"))))

  (it "puts back the change hooks of the buffer alone after a refused edit"
    ;; Not those of a `let' around the edit, nor in the default value
    ;; once a major mode killed the local one
    (with-temp-buffer
      (insert "HEAD\nTAIL")
      (let ((default (default-value 'before-change-functions)))
        (vui-mount-inline (vui-component 'inline-host-form) 6)
        (let ((hooks before-change-functions))
          (let ((before-change-functions
                 (cons #'vui-inline-test--host-hook before-change-functions)))
            (goto-char 8)
            (ignore-errors (insert "x")))
          (run-hooks 'post-command-hook)
          (expect before-change-functions :to-equal hooks))
        (goto-char 8)
        (ignore-errors (insert "x"))
        (fundamental-mode)
        (run-hooks 'post-command-hook)
        (expect (default-value 'before-change-functions) :to-equal default))))

  (it "keeps the guard of a host buffer that has fields of its own"
    (with-temp-buffer
      (widget-insert "HEAD\n")
      (widget-create 'editable-field :size 4 "abc")
      (widget-insert "\nTAIL")
      (widget-setup)
      (let ((form (vui-mount-inline (vui-component 'inline-host-form)
                                    (point-max))))
        (vui-rerender form)
        (goto-char (point-min))
        (expect (insert "x") :to-throw 'text-read-only)
        (run-hooks 'post-command-hook)
        (vui-unmount form)
        (expect (memq #'widget-before-change before-change-functions)
                :to-be-truthy)
        (goto-char (point-min))
        (expect (insert "x") :to-throw 'text-read-only))))

  (it "keeps the guard of a host that makes fields of its own later"
    (with-temp-buffer
      (insert "HEAD\n")
      (let ((form (vui-mount-inline (vui-component 'inline-host-form)
                                    (point-max)))
            (notified nil))
        (goto-char (point-max))
        (widget-insert "\n")
        (widget-create 'editable-field :size 4
                       :notify (lambda (&rest _) (setq notified t))
                       "abc")
        (widget-insert "\nTAIL")
        (widget-setup)
        (vui-rerender form)
        (goto-char (point-min))
        (expect (insert "x") :to-throw 'text-read-only)
        (run-hooks 'post-command-hook)
        (vui-unmount form)
        (goto-char (point-min))
        (expect (insert "x") :to-throw 'text-read-only)
        (run-hooks 'post-command-hook)
        (search-forward "abc")
        (insert "d")
        (expect notified :to-be-truthy))))

  (it "edits host text after a stream writes a field in place"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (vui-stream-append log (vui-text "a"))
      (with-temp-buffer
        (insert "HEAD\nTAIL")
        (vui-mount-inline (vui-component 'inline-host-log :log log) 6)
        (vui-stream-append log (vui-field :value "" :size 3))
        (goto-char (point-min))
        (insert "> ")
        (expect (buffer-substring (point-min) 3) :to-equal "> "))))

  (it "keeps the text of a vui-mount buffer outside of fields read-only"
    (let ((buffer (vui-instance-buffer
                   (vui-mount (vui-component 'inline-host-form)
                              "*test-inline-host*"))))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (expect (insert "x") :to-throw 'text-read-only))
        (kill-buffer buffer)))))

(provide 'vui-inline-test)
;;; vui-inline-test.el ends here

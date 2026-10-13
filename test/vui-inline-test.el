;;; vui-inline-test.el --- Inline mounting tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for inline mounting (issue #8): vui-mount-inline,
;; region-scoped re-rendering inside host buffers, vui-unmount on
;; inline instances, and teardown integration.

;;; Code:

(require 'buttercup)
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
        (expect (vui-inline-instance-at (point-max)) :to-be nil)))))

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

(provide 'vui-inline-test)
;;; vui-inline-test.el ends here

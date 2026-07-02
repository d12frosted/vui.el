;;; vui-text-button-test.el --- Text button tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; vui renders buttons, checkboxes and selects as `button.el' text
;; buttons rather than `widget.el' push-buttons: they are marker-free (so
;; a bufferful renders in linear time, issue #107) and several times
;; lighter.  Editable fields stay widgets.  These tests lock in the text
;; button model and the unified navigation that spans both.

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "text buttons"
  (it "renders a button as a button.el text button, not a widget"
    (with-temp-buffer
      (vui-render (vui-button "click" :on-click #'ignore) (current-buffer))
      (goto-char (point-min))
      (expect (button-at (point)) :to-be-truthy)
      ;; A text button is not a widget
      (expect (widgetp (button-at (point))) :to-be nil)
      (expect (buffer-string) :to-equal "[click]")))

  (it "leaves no overlays or markers behind (linear render)"
    ;; The whole point of #107: text buttons carry no per-element markers
    ;; or overlays, so N of them stay linear.
    (with-temp-buffer
      (vui-render (apply #'vui-vstack
                         (mapcar (lambda (i)
                                   (vui-button (format "b%d" i) :on-click #'ignore))
                                 (number-sequence 1 20)))
                  (current-buffer))
      ;; No button overlays (text buttons are text properties)
      (expect (seq-filter (lambda (ov) (overlay-get ov 'button))
                          (overlays-in (point-min) (point-max)))
              :to-equal nil)))

  (it "stores label, path and key for cursor tracking"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "Save" :key 'save :on-click #'ignore))
                  (current-buffer))
      (let ((b (button-at (point-min))))
        (expect (button-get b :vui-tag) :to-equal "Save")
        (expect (button-get b :vui-key) :to-equal 'save)
        (expect (button-get b :vui-path) :to-equal '(0)))))

  (it "reports bounds via the button, covering the bracketed label"
    (with-temp-buffer
      (vui-render (vui-button "hello" :on-click #'ignore) (current-buffer))
      (let* ((b (vui--elt-at (point-min)))
             (bounds (vui--widget-bounds b)))
        (expect (car bounds) :to-equal (point-min))
        (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
                :to-equal "[hello]"))))

  (it "fires on-click when activated"
    (let ((hit nil))
      (with-temp-buffer
        (vui-render (vui-button "go" :on-click (lambda () (setq hit t)))
                    (current-buffer))
        (button-activate (button-at (point-min))))
      (expect hit :to-be t)))

  (it "renders a disabled button that stays inert and non-clickable-looking"
    (let ((hit nil))
      (with-temp-buffer
        (vui-render (vui-button "no" :disabled t
                                :on-click (lambda () (setq hit t)))
                    (current-buffer))
        (button-activate (button-at (point-min)))
        (expect hit :to-be nil)
        (expect (get-text-property (point-min) 'face) :to-equal 'widget-inactive)
        ;; a disabled button must not advertise clickability
        (expect (get-text-property (point-min) 'mouse-face) :to-be nil)
        (expect (get-text-property (point-min) 'follow-link) :to-be nil))))

  (it "an enabled button shows the clickable affordance"
    (with-temp-buffer
      (vui-render (vui-button "go" :on-click #'ignore) (current-buffer))
      (expect (get-text-property (point-min) 'mouse-face) :to-equal 'highlight)
      (expect (get-text-property (point-min) 'follow-link) :to-be t))))

(describe "unified navigation (vui-forward/backward)"
  (it "moves across text buttons"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (vui-forward 1)
      (expect (button-get (button-at (point)) :vui-tag) :to-equal "two")
      ;; wraps back to the first
      (vui-forward 1)
      (expect (button-get (button-at (point)) :vui-tag) :to-equal "one")))

  (it "collects and navigates adjacent buttons (no separator)"
    ;; Two buttons that abut with no separating text share one contiguous
    ;; `button' text-property span; `next-button' skips the second, so
    ;; collection and navigation must not depend on it or a button is dropped.
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 0
                    (vui-button "a" :key 'a :on-click #'ignore)
                    (vui-button "b" :key 'b :on-click #'ignore)
                    (vui-button "c" :key 'c :on-click #'ignore))
                  (current-buffer))
      (expect (buffer-string) :to-equal "[a][b][c]")
      ;; every button is collected, in order
      (expect (mapcar (lambda (e) (button-get e :vui-key)) (vui--collect-widgets))
              :to-equal '(a b c))
      ;; and every one is reachable by TAB, wrapping
      (goto-char (point-min))
      (let (seq)
        (dotimes (_ 4)
          (vui-forward 1)
          (push (button-get (button-at (point)) :vui-key) seq))
        (expect (nreverse seq) :to-equal '(b c a b)))))

  (it "stops on both text buttons and editable fields, in order"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "top" :on-click #'ignore)
                              (vui-field :size 6 :key 'f)
                              (vui-button "bot" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (let ((visited nil))
        (dotimes (_ 3)
          (vui-forward 1)
          (push (let ((elt (vui--elt-at (point))))
                  (if (widgetp elt) 'field (button-get elt :vui-tag)))
                visited))
        ;; from the first button: field, bot, then wraps to top
        (expect (nreverse visited) :to-equal '(field "bot" "top")))))

  (it "skips buttons with :tab-order -1"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "a" :on-click #'ignore)
                              (vui-button "skip" :on-click #'ignore :tab-order -1)
                              (vui-button "b" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (vui-forward 1)
      ;; "skip" is not tabbable, so we land on "b"
      (expect (button-get (button-at (point)) :vui-tag) :to-equal "b")))

  (it "moves backward and wraps"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      ;; backward from the first wraps to the last
      (vui-backward 1)
      (expect (button-get (button-at (point)) :vui-tag) :to-equal "two"))))

(describe "navigation keys bound on a button"
  ;; Regression: `button-map' inherits `button-buffer-map', which binds TAB
  ;; ([?\t]) to `forward-button' and <backtab> to `backward-button' - a
  ;; button-only walk that skips fields.  When point is on a text button that
  ;; keymap shadows `vui-mode-map', hijacking vui's unified navigation.  vui
  ;; must rebind TAB/S-TAB on its buttons back to `vui-forward'/`vui-backward'.
  (it "resolves TAB and S-TAB to vui nav when point is on a button"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (expect (button-at (point)) :to-be-truthy)
      (expect (key-binding (kbd "TAB")) :to-be 'vui-forward)
      (expect (key-binding (kbd "<tab>")) :to-be 'vui-forward)
      (expect (key-binding (kbd "<backtab>")) :to-be 'vui-backward)
      (expect (key-binding (kbd "S-TAB")) :to-be 'vui-backward)
      ;; RET still activates the button (from button-map underneath)
      (expect (key-binding (kbd "RET")) :to-be 'push-button)))

  (it "reaches a field via the real TAB key from a button (not button-only)"
    ;; Driving whatever TAB is actually bound to must cross the field; the
    ;; button-only `forward-button' would skip it.
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "top" :on-click #'ignore)
                              (vui-field :size 4 :key 'f)
                              (vui-button "bot" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (let ((visited nil))
        (dotimes (_ 3)
          (call-interactively (key-binding (kbd "TAB")))
          (push (let ((e (vui--elt-at (point))))
                  (if (widgetp e) 'field (and e (button-get e :vui-tag))))
                visited))
        (expect (memq 'field visited) :to-be-truthy)
        (expect (nreverse visited) :to-equal '(field "bot" "top"))))))

(describe "keymap bindings survive a reload"
  ;; The nav keys are installed by `vui--install-keymap-keys' at top level, not
  ;; inside the `defvar', so reloading vui.el (which does not re-evaluate a
  ;; bound `defvar') still repairs a stale `vui-mode-map'.  Without this, TAB on
  ;; plain text - which relies on `vui-mode-map', unlike a button that carries
  ;; its own keymap - silently fell back to `indent-for-tab-command'.
  (it "installs TAB/S-TAB on vui-mode-map"
    (expect (lookup-key vui-mode-map (kbd "TAB")) :to-be 'vui-forward)
    (expect (lookup-key vui-mode-map (kbd "<tab>")) :to-be 'vui-forward)
    (expect (lookup-key vui-mode-map (kbd "<backtab>")) :to-be 'vui-backward)
    (expect (lookup-key vui-mode-map (kbd "S-TAB")) :to-be 'vui-backward))

  (it "reinstalls bindings a stale keymap would have lost"
    (define-key vui-mode-map (kbd "TAB") nil)
    (let ((stale (lookup-key vui-mode-map (kbd "TAB"))))
      ;; Reinstall before asserting, so a failure can't leave the map stale
      (vui--install-keymap-keys)
      (expect stale :to-be nil)
      (expect (lookup-key vui-mode-map (kbd "TAB")) :to-be 'vui-forward)))

  (it "binds TAB to vui navigation on plain text in a mounted buffer"
    ;; The user-visible symptom: TAB from a text position (not a button) must
    ;; navigate, which only works if `vui-mode-map' carries the binding.
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "heading")
                              (vui-button "go" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (expect (button-at (point)) :to-be nil)
      (expect (key-binding (kbd "<tab>")) :to-be 'vui-forward)
      (expect (key-binding (kbd "TAB")) :to-be 'vui-forward))))

(describe "Shift-Tab variants and command remapping"
  ;; Shift+Tab reaches Emacs as different events on different platforms
  ;; (<backtab>, S-TAB, <S-tab>, <S-iso-lefttab>...).  Rather than chase every
  ;; representation, vui remaps widget.el's and button.el's own navigation
  ;; commands to its own, so ANY key that would run them navigates the vui way.
  ;; Regression: on some systems Shift+Tab arrives as <S-tab>, which vui did
  ;; not bind, so it leaked through `widget-keymap' to `widget-backward' - which
  ;; jumps to point-max when it finds no widgets, and got stuck there.
  (it "remaps widget/button nav commands to vui nav"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (expect (command-remapping 'widget-forward) :to-be 'vui-forward)
      (expect (command-remapping 'widget-backward) :to-be 'vui-backward)
      (expect (command-remapping 'forward-button) :to-be 'vui-forward)
      (expect (command-remapping 'backward-button) :to-be 'vui-backward)))

  (it "resolves the <S-tab> Shift-Tab variant to vui-backward"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore))
                  (current-buffer))
      ;; on a button
      (goto-char (point-min))
      (expect (key-binding (kbd "<S-tab>")) :to-be 'vui-backward)
      (expect (key-binding (kbd "<backtab>")) :to-be 'vui-backward)
      (expect (key-binding (kbd "S-TAB")) :to-be 'vui-backward)
      ;; and on plain text between/after buttons
      (goto-char (point-max))
      (expect (button-at (point)) :to-be nil)
      (expect (key-binding (kbd "<S-tab>")) :to-be 'vui-backward)))

  (it "S-Tab from the first element wraps to the last (never point-max)"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "one" :on-click #'ignore)
                              (vui-button "two" :on-click #'ignore)
                              (vui-button "three" :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))                ; on "one"
      (call-interactively (key-binding (kbd "<S-tab>")))
      (expect (button-get (button-at (point)) :vui-tag) :to-equal "three"))))

(provide 'vui-text-button-test)
;;; vui-text-button-test.el ends here

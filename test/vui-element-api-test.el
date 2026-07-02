;;; vui-element-api-test.el --- Public element-at-point API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; vui renders interactive elements with two mechanisms: `button.el' text
;; buttons (buttons, checkboxes, selects) and `widget.el' editable fields
;; (issues #107/#109).  Consumers must not have to know which - "what vui
;; element is at point, and what are its properties" is a public contract
;; (`vui-element-at', `vui-element-get', `vui-key-at', `vui-activate') that
;; hides the mechanism, so a rendering refactor like #109 stops being a
;; breaking change for them (issue #113).
;;
;; The load-bearing property of every spec below: not one of them names
;; `button-at', `widget-field-at', `button-get' or `widget-get'.  A button
;; and a field are queried through the exact same calls; if a future refactor
;; swaps a mechanism, these keep passing.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-element-api-test--field-start ()
  "Return the start position of the first editable field in the buffer."
  (widget-field-start (car widget-field-list)))

(describe "vui-element-at"
  (it "returns the text button at POS"
    (with-temp-buffer
      (vui-render (vui-button "click" :on-click #'ignore) (current-buffer))
      (let ((elt (vui-element-at (point-min))))
        (expect elt :to-be-truthy)
        (expect (vui-element-get elt :vui-tag) :to-equal "click"))))

  (it "returns the editable field at POS with the same call"
    (with-temp-buffer
      (vui-render (vui-field :size 6 :key 'f) (current-buffer))
      (let ((elt (vui-element-at (vui-element-api-test--field-start))))
        (expect elt :to-be-truthy)
        (expect (vui-element-get elt :vui-key) :to-equal 'f))))

  (it "defaults POS to point"
    (with-temp-buffer
      (vui-render (vui-button "here" :on-click #'ignore) (current-buffer))
      (goto-char (point-min))
      (expect (vui-element-get (vui-element-at) :vui-tag) :to-equal "here")))

  (it "returns nil when POS holds no interactive element"
    (with-temp-buffer
      (vui-render (vui-text "just text") (current-buffer))
      (expect (vui-element-at (point-min)) :to-be nil))))

(describe "vui-element-get"
  (it "reads vui properties off a text button"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "Save" :key 'save :on-click #'ignore))
                  (current-buffer))
      (let ((elt (vui-element-at (point-min))))
        (expect (vui-element-get elt :vui-key) :to-equal 'save)
        (expect (vui-element-get elt :vui-tag) :to-equal "Save")
        (expect (vui-element-get elt :vui-path) :to-equal '(0)))))

  (it "reads vui properties off an editable field with the identical call"
    ;; The caller never learns that the field is a widget and the button is
    ;; not: the same accessor serves both.
    (with-temp-buffer
      (vui-render (vui-field :size 6 :key 'name) (current-buffer))
      (let ((elt (vui-element-at (vui-element-api-test--field-start))))
        (expect (vui-element-get elt :vui-key) :to-equal 'name)))))

(describe "vui-key-at"
  (it "returns the key of the text button at POS"
    (with-temp-buffer
      (vui-render (vui-button "Go" :key 'go :on-click #'ignore) (current-buffer))
      (expect (vui-key-at (point-min)) :to-equal 'go)))

  (it "returns the key of the editable field at POS"
    (with-temp-buffer
      (vui-render (vui-field :size 6 :key 'email) (current-buffer))
      (expect (vui-key-at (vui-element-api-test--field-start)) :to-equal 'email)))

  (it "defaults POS to point"
    (with-temp-buffer
      (vui-render (vui-button "Go" :key 'go :on-click #'ignore) (current-buffer))
      (goto-char (point-min))
      (expect (vui-key-at) :to-equal 'go)))

  (it "returns nil when the element carries no key"
    (with-temp-buffer
      (vui-render (vui-button "Anon" :on-click #'ignore) (current-buffer))
      (expect (vui-key-at (point-min)) :to-be nil)))

  (it "returns nil when POS holds no interactive element"
    (with-temp-buffer
      (vui-render (vui-text "plain") (current-buffer))
      (expect (vui-key-at (point-min)) :to-be nil))))

(describe "vui-activate"
  (it "runs a text button's action (the follow case)"
    (let ((hit nil))
      (with-temp-buffer
        (vui-render (vui-button "go" :on-click (lambda () (setq hit t)))
                    (current-buffer))
        (expect (vui-activate (point-min)) :to-be-truthy))
      (expect hit :to-be t)))

  (it "toggles a checkbox (the toggle case)"
    (let ((new-value :unset))
      (with-temp-buffer
        (vui-render (vui-checkbox :checked nil
                                  :on-change (lambda (v) (setq new-value v)))
                    (current-buffer))
        (vui-activate (point-min)))
      (expect new-value :to-be t)))

  (it "submits an editable field (its :action)"
    (let ((submitted nil))
      (with-temp-buffer
        (vui-render (vui-field :size 6 :key 'f :value "hi"
                               :on-submit (lambda (v) (setq submitted v)))
                    (current-buffer))
        (expect (vui-activate (vui-element-api-test--field-start))
                :to-be-truthy))
      (expect submitted :to-equal "hi")))

  (it "defaults POS to point"
    (let ((hit nil))
      (with-temp-buffer
        (vui-render (vui-button "go" :on-click (lambda () (setq hit t)))
                    (current-buffer))
        (goto-char (point-min))
        (vui-activate))
      (expect hit :to-be t)))

  (it "reports the element but leaves a disabled button inert"
    (let ((hit nil))
      (with-temp-buffer
        (vui-render (vui-button "no" :disabled t
                                :on-click (lambda () (setq hit t)))
                    (current-buffer))
        ;; An element is present, so activate reports it...
        (expect (vui-activate (point-min)) :to-be-truthy)
        ;; ...but the disabled button's own action no-ops.
        (expect hit :to-be nil))))

  (it "returns nil when POS holds no interactive element"
    (with-temp-buffer
      (vui-render (vui-text "plain") (current-buffer))
      (expect (vui-activate (point-min)) :to-be nil))))

(provide 'vui-element-api-test)
;;; vui-element-api-test.el ends here

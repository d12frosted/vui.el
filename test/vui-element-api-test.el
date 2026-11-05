;;; vui-element-api-test.el --- Public element-at-point API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; vui renders interactive elements with two mechanisms: `button.el' text
;; buttons (buttons, checkboxes, selects) and `widget.el' editable fields
;; (issues #107/#109).  Consumers must not have to know which - "what vui
;; element is at point, and what are its properties" is a public contract
;; (`vui-element-at', `vui-element-get', `vui-key-at', `vui-activate',
;; `vui-field-at', `vui-element-bounds', `vui-element-by-key') that
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

(defun vui-element-api-test--pos-of (text)
  "Return the position of the first TEXT in the buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(vui-defcomponent vui-element-api-test-row (label)
  :render (vui-text label))

(describe "vui-key-at on rows of text"
  (it "returns the key of the keyed region at POS"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-region :key 'one (vui-text "first"))
                              (vui-region :key 'two (vui-text "second")))
                  (current-buffer))
      (expect (vui-key-at (vui-element-api-test--pos-of "first")) :to-equal 'one)
      (expect (vui-key-at (vui-element-api-test--pos-of "cond")) :to-equal 'two)))

  (it "returns the key of the keyed component at POS"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-component 'vui-element-api-test-row :key "a" :label "row a")
                   (vui-component 'vui-element-api-test-row :key "b" :label "row b"))
                  (current-buffer))
      (expect (vui-key-at (vui-element-api-test--pos-of "row b")) :to-equal "b")))

  (it "returns the innermost key"
    (with-temp-buffer
      (vui-render (vui-region :key 'outer
                    (vui-vstack (vui-text "head")
                                (vui-region :key 'inner (vui-text "body"))))
                  (current-buffer))
      (expect (vui-key-at (vui-element-api-test--pos-of "head")) :to-equal 'outer)
      (expect (vui-key-at (vui-element-api-test--pos-of "body")) :to-equal 'inner)))

  (it "returns the row's key on a button without one, and a button's own key"
    (with-temp-buffer
      (vui-render (vui-region :key 'row
                    (vui-hstack (vui-button "anon" :on-click #'ignore)
                                (vui-button "own" :key 'btn :on-click #'ignore)))
                  (current-buffer))
      (expect (vui-key-at (vui-element-api-test--pos-of "anon")) :to-equal 'row)
      (expect (vui-key-at (vui-element-api-test--pos-of "own")) :to-equal 'btn)))

  (it "returns the row's key at the end of its line"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-region :key 'one (vui-text "first"))
                              (vui-region :key 'two (vui-text "second")))
                  (current-buffer))
      (goto-char (vui-element-api-test--pos-of "first"))
      (end-of-line)
      (expect (vui-key-at) :to-equal 'one)
      (goto-char (point-max))
      (expect (vui-key-at) :to-equal 'two)))

  (it "returns the row's key at the end of its line inside a keyed section"
    (with-temp-buffer
      (vui-render (vui-region :key 'section
                    (vui-vstack (vui-text "Header")
                                (vui-region :key 'a (vui-text "row a"))
                                (vui-region :key 'b (vui-text "row b"))))
                  (current-buffer))
      (goto-char (vui-element-api-test--pos-of "row a"))
      (end-of-line)
      (expect (vui-key-at) :to-equal 'a)))

  (it "returns the row's key at the start of an indented line"
    (with-temp-buffer
      (vui-render (vui-vstack :indent 2
                    (vui-region :key 'a (vui-text "aa"))
                    (vui-region :key 'b (vui-vstack (vui-text "bb") (vui-text "bb-2"))))
                  (current-buffer))
      (goto-char (point-min))
      (expect (vui-key-at) :to-equal 'a)
      (forward-line 1)
      (expect (vui-key-at) :to-equal 'b)
      (forward-line 1)
      (expect (vui-key-at) :to-equal 'b)))

  (it "puts no key on the text of a keyed buffer root"
    ;; The root holds the whole buffer, and its re-renders leave text as
    ;; it was, so its key would be there or not by chance
    (let ((vui-render-delay nil))
      (vui-mount (vui-component 'vui-element-api-test-row :key 'root :label "all")
                 "*test-keys-root*" t)
      (unwind-protect
          (with-current-buffer "*test-keys-root*"
            (expect (vui-key-at (point-min)) :to-be nil))
        (kill-buffer "*test-keys-root*"))))

  (it "does not give text typed in a field the key of the row before it"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-element-api-test-two-rows ()
        :render (vui-hstack :spacing 0
                 (vui-region :key 'a (vui-text "A:"))
                 (vui-region :key 'b (vui-field :size 4))))
      (vui-mount (vui-component 'vui-element-api-test-two-rows) "*test-keys-typed*" t)
      (unwind-protect
          (with-current-buffer "*test-keys-typed*"
            (let ((start (vui-element-api-test--field-start)))
              (goto-char start)
              (let ((last-command-event ?z))
                (self-insert-command 1))
              (expect (buffer-substring-no-properties start (1+ start)) :to-equal "z")
              (expect (get-text-property start 'vui-key) :not :to-be 'a)))
        (kill-buffer "*test-keys-typed*"))))

  (it "leaves the key out of text yanked in a vui buffer"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-element-api-test-yank ()
        :render (vui-vstack (vui-region :key 'a (vui-text "alpha"))
                            (vui-field :size 8)))
      (vui-mount (vui-component 'vui-element-api-test-yank) "*test-keys-yank*" t)
      (unwind-protect
          (with-current-buffer "*test-keys-yank*"
            (let ((kill-ring nil))
              (kill-new (buffer-substring (point-min) (+ (point-min) 5)))
              (goto-char (vui-element-api-test--field-start))
              (yank)
              (expect (get-text-property (vui-element-api-test--field-start) 'vui-key)
                      :to-be nil)))
        (kill-buffer "*test-keys-yank*"))))

  (it "returns nil on text outside any keyed row"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "title")
                              (vui-region :key 'one (vui-text "first")))
                  (current-buffer))
      (expect (vui-key-at (vui-element-api-test--pos-of "title")) :to-be nil)))

  (it "keeps the keys right across a re-render that reorders the rows"
    (let ((vui-render-delay nil))
      (vui-defcomponent vui-element-api-test-list ()
        :state ((items '("a" "b" "c")))
        :render (apply #'vui-vstack
                       (mapcar (lambda (i)
                                 (vui-component 'vui-element-api-test-row
                                   :key i :label (concat "row " i)))
                               items)))
      (let ((inst (vui-mount (vui-component 'vui-element-api-test-list)
                             "*test-keys-reorder*" t)))
        (unwind-protect
            (with-current-buffer "*test-keys-reorder*"
              (let ((vui--current-instance inst))
                (vui-set-state :items '("c" "a")))
              (expect (buffer-string) :to-equal "row c\nrow a")
              (expect (vui-key-at (vui-element-api-test--pos-of "row c")) :to-equal "c")
              (expect (vui-key-at (vui-element-api-test--pos-of "row a")) :to-equal "a"))
          (kill-buffer "*test-keys-reorder*")))))

  (it "returns the key of a keyed component appended to a stream"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-element-api-test-feed ()
        :render (vui-stream stream))
      (vui-mount (vui-component 'vui-element-api-test-feed) "*test-keys-stream*" t)
      (unwind-protect
          (with-current-buffer "*test-keys-stream*"
            (vui-stream-append stream (vui-component 'vui-element-api-test-row
                                        :key 'first :label "one"))
            (vui-stream-append stream (vui-component 'vui-element-api-test-row
                                        :key 'second :label "two"))
            (expect (vui-key-at (vui-element-api-test--pos-of "two")) :to-equal 'second))
        (kill-buffer "*test-keys-stream*"))))

  (it "leaves the value of a field in a keyed row without the key"
    (let ((vui-render-delay nil)
          (seen nil))
      (vui-defcomponent vui-element-api-test-form ()
        :render (vui-region :key 'row
                  (vui-field :key 'name :size 8
                             :on-change (lambda (v) (setq seen v)))))
      (vui-mount (vui-component 'vui-element-api-test-form) "*test-keys-field*" t)
      (unwind-protect
          (with-current-buffer "*test-keys-field*"
            (goto-char (vui-element-api-test--field-start))
            (insert "abc")
            (expect (vui-field-value 'name) :to-equal "abc")
            (expect (text-properties-at 0 (vui-field-value 'name)) :to-be nil)
            (expect (text-properties-at 0 seen) :to-be nil))
        (kill-buffer "*test-keys-field*")))))

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

;; Issue #291: fields, their bounds, and any element by key

(defun vui-element-api-test--field-end (key)
  "Return the end of the editable area of the field with KEY."
  (cdr (vui-element-bounds (vui-element-by-key key))))

(describe "vui-field-at"
  (it "returns the field at POS"
    (with-temp-buffer
      (vui-render (vui-field :size 6 :key 'f :value "ab") (current-buffer))
      (let ((field (vui-field-at (1+ (vui-element-api-test--field-start)))))
        (expect field :to-be-truthy)
        (expect (vui-element-get field :vui-key) :to-equal 'f))))

  (it "never returns a button"
    (with-temp-buffer
      (vui-render (vui-button "click" :on-click #'ignore) (current-buffer))
      (expect (vui-element-at (point-min)) :to-be-truthy)
      (expect (vui-field-at (point-min)) :to-be nil)))

  (it "counts the end of a full field, where typing still goes into it"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 0
                              (vui-field :size 4 :key 'f :value "abcd")
                              (vui-text "|after"))
                  (current-buffer))
      (let ((end (vui-element-api-test--field-end 'f)))
        (expect (buffer-substring-no-properties end (1+ end)) :to-equal "|")
        ;; vui-element-at looks at the character after POS and finds none
        (expect (vui-element-at end) :to-be nil)
        (expect (vui-element-get (vui-field-at end) :vui-key) :to-equal 'f))))

  (it "ignores a widget.el field the host buffer made itself"
    (with-temp-buffer
      (widget-create 'editable-field :size 4 "abcd")
      (widget-setup)
      (let ((start (widget-field-start (car widget-field-list))))
        (expect (vui-field-at start) :to-be nil)
        (expect (vui-field-at (+ start 4)) :to-be nil))))

  (it "ignores the field property a host buffer uses for its own text"
    (with-temp-buffer
      (insert (propertize "prompt> " 'field 'prompt))
      (expect (vui-field-at 2) :to-be nil)))

  (it "defaults POS to point"
    (with-temp-buffer
      (vui-render (vui-hstack :spacing 0
                              (vui-field :size 4 :key 'f :value "abcd")
                              (vui-text "|after"))
                  (current-buffer))
      ;; at the end of the full field, where only the end lookup finds it
      (goto-char (vui-element-api-test--field-end 'f))
      (expect (vui-element-get (vui-field-at) :vui-key) :to-equal 'f)))

  (it "returns nil on plain text"
    (with-temp-buffer
      (vui-render (vui-text "plain") (current-buffer))
      (expect (vui-field-at (point-min)) :to-be nil))))

(describe "vui-element-bounds"
  (it "covers a field's editable area, padding included"
    (with-temp-buffer
      (vui-render (vui-hstack (vui-text "q:") (vui-field :size 6 :key 'f :value "ab"))
                  (current-buffer))
      (let ((bounds (vui-element-bounds (vui-element-by-key 'f))))
        (expect (car bounds) :to-equal (vui-element-api-test--field-start))
        (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
                :to-equal "ab    ")
        ;; the end of the value is the start plus its length
        (expect (buffer-substring-no-properties
                 (car bounds) (+ (car bounds) (length (vui-field-value 'f))))
                :to-equal "ab"))))

  (it "covers a button's whole text, brackets included"
    (with-temp-buffer
      (vui-render (vui-hstack (vui-text "x") (vui-button "click" :key 'b :on-click #'ignore))
                  (current-buffer))
      (let ((bounds (vui-element-bounds (vui-element-by-key 'b))))
        (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
                :to-equal "[click]"))))

  (it "returns nil for no element"
    (expect (vui-element-bounds nil) :to-be nil)))

(describe "vui-element-by-key"
  (it "finds a field and a button with the same call"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-field :size 6 :key 'query)
                              (vui-button "go" :key 'go :on-click #'ignore))
                  (current-buffer))
      (expect (vui-element-get (vui-element-by-key 'query) :vui-key) :to-equal 'query)
      (expect (vui-element-get (vui-element-by-key 'go) :vui-tag) :to-equal "go")))

  (it "compares keys with equal"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "x" :key (list 'note "1") :on-click #'ignore)
                              (vui-button "y" :key (list 'note "2") :on-click #'ignore))
                  (current-buffer))
      (expect (vui-element-get (vui-element-by-key (list 'note "2")) :vui-tag)
              :to-equal "y")))

  (it "returns the first element with KEY in buffer order"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "first" :key 'k :on-click #'ignore)
                              (vui-field :size 4 :key 'k))
                  (current-buffer))
      (expect (vui-element-get (vui-element-by-key 'k) :vui-tag) :to-equal "first")))

  (it "searches only between START and END"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-button "a" :key 'k :on-click #'ignore)
                              (vui-button "b" :key 'k :on-click #'ignore))
                  (current-buffer))
      (goto-char (point-min))
      (forward-line 1)
      (expect (vui-element-get (vui-element-by-key 'k (point)) :vui-tag) :to-equal "b")
      (expect (vui-element-by-key 'k (point-min) 2) :to-be-truthy)
      (expect (vui-element-by-key 'k (point) (point)) :to-be nil)))

  (it "treats a nil key as a miss, not a match on unkeyed elements"
    (with-temp-buffer
      (vui-render (vui-button "plain" :on-click #'ignore) (current-buffer))
      (expect (vui-element-by-key nil) :to-be nil)))

  (it "returns nil when no element carries KEY"
    (with-temp-buffer
      (vui-render (vui-button "x" :key 'a :on-click #'ignore) (current-buffer))
      (expect (vui-element-by-key 'missing) :to-be nil))))

(provide 'vui-element-api-test)
;;; vui-element-api-test.el ends here

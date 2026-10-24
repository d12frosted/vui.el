;;; vui-field-edge-test.el --- Fields at the edge of kept text -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A field's overlay takes in text inserted at either of its ends: this
;; is what makes typing at the edge of a field work.  These specs are
;; about a field that sits at the edge of text a render keeps (a
;; `vui-stream' item, a component that bails out of the component
;; patch, the host text around a `vui-mount-inline' region):
;;
;; - text a render puts next to such a field is not part of the field;
;; - a region that ends (or starts) with a field follows the edits made
;;   at that edge of the field;
;; - a field of the host buffer that touches an inline region stays a
;;   field of the host buffer.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

(defun vui-fet--field (key)
  "Return the field with KEY from `widget-field-list'."
  (seq-find (lambda (w) (equal (widget-get w :vui-key) key))
            widget-field-list))

(defun vui-fet--text (field)
  "Return the buffer text FIELD covers, nil when FIELD is dead or unknown.
FIELD is a widget, or the key of a field in `widget-field-list'."
  (when-let* ((w (if (consp field) field (vui-fet--field field)))
              (start (widget-field-start w))
              (end (widget-field-end w)))
    (buffer-substring-no-properties start end)))

(defun vui-fet--kill (buf)
  "Kill BUF without running its field hooks."
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-fet--settle (instance)
  "Wait for the render INSTANCE has scheduled."
  (let ((deadline (+ (float-time) 10)))
    (while (and (vui-instance-render-timer instance)
                (< (float-time) deadline))
      (sleep-for 0.01))))

(defmacro vui-fet--with-chat (spec &rest body)
  "Run BODY in a buffer with a stream UI mounted.
SPEC is (STREAM INSTANCE COMPONENT [SEPARATOR]): the stream handle and
the root instance are bound to STREAM and INSTANCE, COMPONENT is
mounted with the stream and the label \"a\"."
  (declare (indent 1))
  (pcase-let ((`(,stream ,instance ,component ,separator) spec))
    `(let* ((vui-render-delay nil)
            (,stream (vui-make-stream ,separator))
            (,instance (vui-mount (vui-component ,component
                                                 :stream ,stream :label "a")
                                  "*vui-fet*")))
       (ignore ,instance)
       (unwind-protect
           (with-current-buffer "*vui-fet*" ,@body)
         (vui-fet--kill "*vui-fet*")))))

(defun vui-fet--item (key value &optional label)
  "Return a stream item that ends with a field of three characters.
The field has KEY and VALUE.  LABEL (default: KEY's name) goes before
it."
  (vui-fragment (vui-text (format "%s " (or label key)))
                (vui-field :key key :value value :size 3)))

;;; Components

(vui-defcomponent vui-fet-chat (stream label)
  "A stream with a status line under it."
  :render (vui-vstack (vui-stream stream)
                      (vui-text (format "status: %s" label))))

(vui-defcomponent vui-fet-chat-input (stream label)
  "A stream with an input line and a status line under it."
  :render (vui-vstack (vui-stream stream)
                      (vui-hstack (vui-text "> ")
                                  (vui-field :key 'input :value "hi" :size 4))
                      (vui-text (format "status: %s" label))))

(vui-defcomponent vui-fet-row (label)
  "A row with no field."
  :render (vui-text (format "[%s]" label)))

(vui-defcomponent vui-fet-item (id label)
  "A line that ends with a field.  It renders again only for a new LABEL."
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-fragment (vui-text (format "%s " label))
                        (vui-field :key id :value "x" :size 3)))

(vui-defcomponent vui-fet-list (items)
  "One `vui-fet-item' per line."
  :render (apply #'vui-vstack
                 (mapcar (lambda (it)
                           (vui-component 'vui-fet-item
                             :key (car it) :id (car it) :label (cdr it)))
                         items)))

(vui-defcomponent vui-fet-cell (id label)
  "A cell that starts with a field.  It renders again only for a new LABEL."
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-fragment (vui-field :key id :value "x" :size 3)
                        (vui-text (format " %s;" label))))

(vui-defcomponent vui-fet-hint-cell (id label)
  "A cell that starts with an empty field showing a placeholder."
  :should-update (not (equal label (plist-get prev-props :label)))
  :render (vui-fragment (vui-field :key id :value "" :size 3
                                   :placeholder "???")
                        (vui-text (format " %s;" label))))

(vui-defcomponent vui-fet-hint-cells (items)
  "The `vui-fet-hint-cell's of one line, with nothing between them."
  :render (apply #'vui-fragment
                 (mapcar (lambda (it)
                           (vui-component 'vui-fet-hint-cell
                             :key (car it) :id (car it) :label (cdr it)))
                         items)))

(vui-defcomponent vui-fet-cells (items)
  "The `vui-fet-cell's of one line, with nothing between them."
  :render (apply #'vui-fragment
                 (mapcar (lambda (it)
                           (vui-component 'vui-fet-cell
                             :key (car it) :id (car it) :label (cdr it)))
                         items)))

(vui-defcomponent vui-fet-word ()
  "A full field that is the whole region and follows what is typed."
  :state ((text "abcd"))
  :render (vui-field :key 'word :value text :size 4
                     :on-change (lambda (value) (vui-set-state :text value))))

(vui-defcomponent vui-fet-plain-word (label)
  "A full field that ends the region and re-renders only for a new LABEL."
  :render (vui-fragment (vui-text (or label "L:"))
                        (vui-field :key 'word :value "abcd" :size 4)))

;;; Text a render puts next to a field it keeps

(describe "a field that ends a stream item"
  (it "does not take in an item appended after it"
    (vui-fet--with-chat (s _inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "x"))
      (vui-stream-append s (vui-text "m1"))
      (expect (buffer-string) :to-equal "m0 x  \nm1\nstatus: a")
      (expect (vui-fet--text 'm0) :to-equal "x  ")))

  (it "does not take in a component row appended after it"
    (vui-fet--with-chat (s _inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "x"))
      (vui-stream-append s (vui-component 'vui-fet-row :label "r"))
      (expect (buffer-string) :to-equal "m0 x  \n[r]\nstatus: a")
      (expect (vui-fet--text 'm0) :to-equal "x  ")))

  (it "does not take in the content after the stream when it re-renders"
    (vui-fet--with-chat (s inst 'vui-fet-chat-input)
      (vui-stream-append s (vui-fet--item 'm0 "x"))
      (vui-stream-append s (vui-fet--item 'm1 ""))
      (dolist (label '("b" "cc" "ddd"))
        (vui-update inst (list :stream s :label label))
        (expect (buffer-string)
                :to-equal (format "m0 x  \nm1    \n>  hi  \nstatus: %s" label))
        (expect (vui-fet--text 'm0) :to-equal "x  ")
        (expect (vui-fet--text 'm1) :to-equal "   ")
        (expect (vui-fet--text 'input) :to-equal "hi  ")
        (expect (length widget-field-list) :to-equal 3))
      ;; and no field is left behind without bounds
      (expect (widget-field-find (point-max)) :to-be nil)))

  (it "still takes what is typed at its end"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "xyz"))
      (vui-stream-append s (vui-text "m1"))
      (vui-update inst (list :stream s :label "b"))
      (goto-char (widget-field-end (vui-fet--field 'm0)))
      (insert "w")
      (expect (buffer-string) :to-equal "m0 xyzw\nm1\nstatus: b")
      (expect (vui-field-value 'm0) :to-equal "xyzw")))

  (it "does not take in a node inserted after its node"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-text "top"))
      (let ((node (vui-stream-open s (vui-fet--item 'n "x"))))
        ;; the field of an item written into a live stream is set up by
        ;; the next render
        (vui-update inst (list :stream s :label "b"))
        (vui-stream-after node (vui-text "below"))
        (expect (buffer-string) :to-equal "top\nn x  \nbelow\nstatus: b")
        (expect (vui-fet--text 'n) :to-equal "x  "))))

  (it "does not take in text appended to its node"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-text "top"))
      (let ((node (vui-stream-open s (vui-fet--item 'n "x"))))
        (vui-update inst (list :stream s :label "b"))
        (vui-stream-append-to node (vui-text " more"))
        (expect (buffer-string) :to-equal "top\nn x   more\nstatus: b")
        (expect (vui-fet--text 'n) :to-equal "x  "))))

  (it "does not take in the next item when that item is replaced"
    ;; No separator: the next item starts where the field ends.
    (vui-fet--with-chat (s inst 'vui-fet-chat "")
      (vui-stream-append s (vui-fet--item 'm0 "x"))
      (let ((node (vui-stream-open s (vui-text "one"))))
        (vui-update inst (list :stream s :label "b"))
        (vui-stream-update node (vui-text "two"))
        (expect (buffer-string) :to-equal "m0 x  two\nstatus: b")
        (expect (vui-fet--text 'm0) :to-equal "x  ")))))

(describe "a field that starts a stream item"
  (it "does not take in a node inserted before its node"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-text "top"))
      (let ((node (vui-stream-open
                   s (vui-fragment (vui-field :key 'n :value "x" :size 3)
                                   (vui-text " n")))))
        (vui-update inst (list :stream s :label "b"))
        (vui-stream-before node (vui-text "above"))
        (expect (buffer-string) :to-equal "top\nabove\nx   n\nstatus: b")
        (expect (vui-fet--text 'n) :to-equal "x  ")))))

(describe "a field at the edge of a component that bails out"
  (it "does not take in the component re-rendered after it"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-fet-list
                               :items '((a . "one") (b . "two")))
                             "*vui-fet*")))
        (unwind-protect
            (with-current-buffer "*vui-fet*"
              (dolist (label '("TWO" "Two" "2"))
                (vui-update inst `(:items ((a . "one") (b . ,label))))
                (expect (buffer-string)
                        :to-equal (format "one x  \n%s x  " label))
                (expect (vui-fet--text 'a) :to-equal "x  ")
                (expect (vui-fet--text 'b) :to-equal "x  ")
                (expect (length widget-field-list) :to-equal 2)))
          (vui-fet--kill "*vui-fet*")))))

  (it "does not take in the component re-rendered before it"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-fet-cells
                               :items '((a . "one") (b . "two")))
                             "*vui-fet*")))
        (unwind-protect
            (with-current-buffer "*vui-fet*"
              (dolist (label '("ONE" "One" "1"))
                (vui-update inst `(:items ((a . ,label) (b . "two"))))
                (expect (buffer-string)
                        :to-equal (format "x   %s;x   two;" label))
                (expect (vui-fet--text 'a) :to-equal "x  ")
                (expect (vui-fet--text 'b) :to-equal "x  ")
                (expect (length widget-field-list) :to-equal 2)))
          (vui-fet--kill "*vui-fet*")))))

  (it "does not hide the component re-rendered before it behind its placeholder"
    (let ((vui-incremental-render t)
          (vui-render-delay nil))
      (let ((inst (vui-mount (vui-component 'vui-fet-hint-cells
                               :items '((a . "one") (b . "two")))
                             "*vui-fet*")))
        (unwind-protect
            (with-current-buffer "*vui-fet*"
              (dolist (label '("ONE" "One" "1"))
                (vui-update inst `(:items ((a . ,label) (b . "two"))))
                (expect (buffer-string)
                        :to-equal (format "    %s;    two;" label))
                ;; each placeholder covers its field and nothing else
                (let ((covered
                       (mapcar (lambda (ov)
                                 (cons (overlay-start ov) (overlay-end ov)))
                               (seq-filter
                                (lambda (ov) (overlay-get ov 'vui-placeholder))
                                (overlays-in (point-min) (point-max)))))
                      (fields
                       (mapcar (lambda (w)
                                 (cons (widget-field-start w)
                                       (widget-field-end w)))
                               widget-field-list)))
                  (expect (length fields) :to-equal 2)
                  (expect (sort covered #'car-less-than-car)
                          :to-equal (sort fields #'car-less-than-car)))))
          (vui-fet--kill "*vui-fet*"))))))

;;; Fields of the host buffer around an inline region

(describe "a host field next to an inline region"
  (it "that ends where the region starts stays as it is"
    (with-temp-buffer
      (insert "HEAD ")
      (let ((host (widget-create 'editable-field :size 4 "host"))
            (pos (point)))
        (insert "\nTAIL")
        (widget-setup)
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-fet-row :label "a") pos)))
          (expect (buffer-string) :to-equal "HEAD host[a]\nTAIL")
          (expect (vui-fet--text host) :to-equal "host")
          (dolist (label '("bb" "c"))
            (vui-update instance (list :label label))
            (expect (buffer-string)
                    :to-equal (format "HEAD host[%s]\nTAIL" label))
            (expect (vui-fet--text host) :to-equal "host")
            (expect (memq host widget-field-list) :to-be-truthy))
          (vui-unmount instance)
          (expect (buffer-string) :to-equal "HEAD host\nTAIL")
          (expect (vui-fet--text host) :to-equal "host")))))

  (it "that starts where the region ends stays as it is"
    (with-temp-buffer
      (insert "HEAD ")
      (let ((pos (point))
            (host (widget-create 'editable-field :size 4 "host")))
        (insert "\nTAIL")
        (widget-setup)
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-fet-row :label "a") pos)))
          (expect (buffer-string) :to-equal "HEAD [a]host\nTAIL")
          (expect (vui-fet--text host) :to-equal "host")
          (expect (memq host widget-field-list) :to-be-truthy)
          (dolist (label '("bb" "c"))
            (vui-update instance (list :label label))
            (expect (buffer-string)
                    :to-equal (format "HEAD [%s]host\nTAIL" label))
            (expect (vui-fet--text host) :to-equal "host")
            (expect (memq host widget-field-list) :to-be-truthy))
          (vui-unmount instance)
          (expect (buffer-string) :to-equal "HEAD host\nTAIL")
          (expect (vui-fet--text host) :to-equal "host")
          (expect (memq host widget-field-list) :to-be-truthy)))))

  (it "on each side of the region stays as it is"
    (with-temp-buffer
      (insert "HEAD ")
      (let* ((before (widget-create 'editable-field :size 3 "one"))
             (pos (point))
             (after (widget-create 'editable-field :size 3 "two")))
        (insert "\nTAIL")
        (widget-setup)
        (let ((instance (vui-mount-inline
                         (vui-component 'vui-fet-row :label "a") pos)))
          (dolist (label '("bb" "c"))
            (vui-update instance (list :label label))
            (expect (buffer-string)
                    :to-equal (format "HEAD one[%s]two\nTAIL" label))
            (expect (vui-fet--text before) :to-equal "one")
            (expect (vui-fet--text after) :to-equal "two")
            (expect (length widget-field-list) :to-equal 2)))))))

;;; Edits at the edge of a field that is the edge of a region

(describe "a field that ends an inline region"
  (it "keeps a character typed at its end inside the region"
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (let ((instance (vui-mount-inline (vui-component 'vui-fet-word) 6)))
          (goto-char (widget-field-end (vui-fet--field 'word)))
          (insert "e")
          (vui-fet--settle instance)
          (expect (buffer-string) :to-equal "HEAD\nabcde\nTAIL")
          (expect (buffer-substring (point-min) (point))
                  :to-equal "HEAD\nabcde")
          (expect (vui-fet--text 'word) :to-equal "abcde")))))

  (it "keeps the padding of a deleted character inside the region"
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (let ((instance (vui-mount-inline (vui-component 'vui-fet-word) 6)))
          (goto-char (widget-field-end (vui-fet--field 'word)))
          (delete-char -1)
          (vui-fet--settle instance)
          (expect (buffer-string) :to-equal "HEAD\nabc \nTAIL")
          (delete-char -1)
          (vui-fet--settle instance)
          (expect (buffer-string) :to-equal "HEAD\nab  \nTAIL")
          (expect (vui-fet--text 'word) :to-equal "ab  ")))))

  (it "takes what was typed at its end with it when the region is removed"
    (with-temp-buffer
      (insert "HEAD\n\nTAIL")
      (let ((instance (vui-mount-inline (vui-component 'vui-fet-plain-word) 6)))
        (goto-char (widget-field-end (vui-fet--field 'word)))
        (insert "e")
        (expect (buffer-string) :to-equal "HEAD\nL:abcde\nTAIL")
        (vui-unmount instance)
        (expect (buffer-string) :to-equal "HEAD\n\nTAIL"))))

  (it "takes its padding with it when the region re-renders"
    (with-temp-buffer
      (insert "HEAD\n\nTAIL")
      (let ((instance (vui-mount-inline (vui-component 'vui-fet-plain-word) 6)))
        (goto-char (widget-field-end (vui-fet--field 'word)))
        (delete-char -1)
        (expect (buffer-string) :to-equal "HEAD\nL:abc \nTAIL")
        (vui-update instance '(:label "M:"))
        (expect (buffer-string) :to-equal "HEAD\nM:abcd\nTAIL")
        (vui-unmount instance)
        (expect (buffer-string) :to-equal "HEAD\n\nTAIL")))))

(describe "a field that starts an inline region"
  (it "keeps a character typed at its start inside the region"
    (let ((vui-render-delay 0.01))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (let ((instance (vui-mount-inline (vui-component 'vui-fet-word) 6)))
          (goto-char (widget-field-start (vui-fet--field 'word)))
          (insert "Z")
          (vui-fet--settle instance)
          (expect (buffer-string) :to-equal "HEAD\nZabcd\nTAIL")
          (expect (vui-fet--text 'word) :to-equal "Zabcd"))))))

(describe "a field that ends the last item of a stream"
  (it "keeps its padding before an item appended after a deletion"
    (vui-fet--with-chat (s _inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "xyz"))
      (goto-char (widget-field-end (vui-fet--field 'm0)))
      (delete-char -1)
      (expect (buffer-string) :to-equal "m0 xy \nstatus: a")
      (vui-stream-append s (vui-text "m1"))
      (expect (buffer-string) :to-equal "m0 xy \nm1\nstatus: a")
      (expect (vui-fet--text 'm0) :to-equal "xy ")))

  (it "keeps a character typed at its end before an appended item"
    (vui-fet--with-chat (s _inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "xyz"))
      (goto-char (widget-field-end (vui-fet--field 'm0)))
      (insert "w")
      (vui-stream-append s (vui-text "m1"))
      (expect (buffer-string) :to-equal "m0 xyzw\nm1\nstatus: a")
      (expect (vui-fet--text 'm0) :to-equal "xyzw")))

  (it "keeps its padding when the content after the stream re-renders"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-fet--item 'm0 "xyz"))
      (goto-char (widget-field-end (vui-fet--field 'm0)))
      (delete-char -1)
      (vui-update inst (list :stream s :label "b"))
      (expect (buffer-string) :to-equal "m0 xy \nstatus: b")
      (expect (vui-fet--text 'm0) :to-equal "xy ")))

  (it "keeps its padding before an item appended after its row"
    ;; A row is an inline region of its own, and its end is the end of
    ;; the stream while it is the last item.
    (vui-fet--with-chat (s _inst 'vui-fet-chat)
      (vui-stream-append s (vui-text "top"))
      (vui-stream-append s (vui-component 'vui-fet-plain-word))
      (goto-char (widget-field-end (vui-fet--field 'word)))
      (delete-char -1)
      (expect (buffer-string) :to-equal "top\nL:abc \nstatus: a")
      (vui-stream-append s (vui-text "m1"))
      (expect (buffer-string) :to-equal "top\nL:abc \nm1\nstatus: a")
      (expect (vui-fet--text 'word) :to-equal "abc ")))

  (it "keeps its padding before text appended to its node"
    (vui-fet--with-chat (s inst 'vui-fet-chat)
      (vui-stream-append s (vui-text "top"))
      (let ((node (vui-stream-open s (vui-fet--item 'n "xyz"))))
        (vui-update inst (list :stream s :label "b"))
        (goto-char (widget-field-end (vui-fet--field 'n)))
        (delete-char -1)
        (vui-stream-append-to node (vui-text "!"))
        (expect (buffer-string) :to-equal "top\nn xy !\nstatus: b")
        (expect (vui-fet--text 'n) :to-equal "xy ")))))

;;; RET at the end of a field

(defvar vui-fet--submitted nil
  "The values the `on-submit' of `vui-fet-name' got, the latest first.")

(vui-defcomponent vui-fet-name (submit)
  "A field that ends its line, and the name it holds.
With SUBMIT, its `on-submit' pushes to `vui-fet--submitted'."
  :state ((name ""))
  :render (vui-vstack
           (vui-hstack
            (vui-text "Name:")
            (vui-field :key 'name :value name :size 6
                       :on-change (lambda (value) (vui-set-state :name value))
                       :on-submit (when submit
                                    (lambda (value)
                                      (push value vui-fet--submitted)))))
           (vui-text (format "Hello, %s!" name))))

(defun vui-fet--type-name ()
  "Type a name in the `name' field, go to the end of the line, press RET.
Keys go through the command loop as if typed, so the current buffer is
shown in the selected window first."
  (switch-to-buffer (current-buffer))
  (goto-char (widget-field-start (vui-fet--field 'name)))
  (execute-kbd-macro (kbd "B o b C-e"))
  (expect (point) :to-equal (widget-field-end (vui-fet--field 'name)))
  (execute-kbd-macro (kbd "RET")))

(vui-defcomponent vui-fet-submit-row (value)
  "A field with VALUE, then text on its line.
Its `on-submit' pushes to `vui-fet--submitted'."
  :render (vui-hstack
           (vui-field :key 'row :value value :size 4
                      :on-submit (lambda (v) (push v vui-fet--submitted)))
           (vui-text "tail")))

(vui-defcomponent vui-fet-submit-pair ()
  "Two fields with no text between them, the first one full.
Their `on-submit's push (KEY VALUE) to `vui-fet--submitted'."
  :render (vui-hstack
           :spacing 0
           (vui-field :key 'a :value "aaa" :size 3
                      :on-submit (lambda (v) (push (list 'a v) vui-fet--submitted)))
           (vui-field :key 'b :value "bb" :size 3
                      :on-submit (lambda (v) (push (list 'b v) vui-fet--submitted)))))

(describe "RET at the end of a field"
  (before-each (setq vui-fet--submitted nil))

  (it "submits the field in a buffer of vui-mount"
    (let ((vui-render-delay nil))
      (save-window-excursion
        (unwind-protect
            (with-current-buffer (vui-instance-buffer
                                  (vui-mount (vui-component 'vui-fet-name
                                                            :submit t)
                                             "*vui-fet-ret*"))
              (vui-fet--type-name)
              (expect vui-fet--submitted :to-equal '("Bob"))
              (expect (vui-fet--text 'name) :to-equal "Bob   ")
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "Name: Bob   \nHello, Bob!"))
          (vui-fet--kill "*vui-fet-ret*")))))

  (it "submits the field of an inline region"
    (let ((vui-render-delay nil))
      (save-window-excursion
        (with-temp-buffer
          (insert "HEAD\n\nTAIL")
          (vui-mount-inline (vui-component 'vui-fet-name :submit t) 6)
          (vui-fet--type-name)
          (expect vui-fet--submitted :to-equal '("Bob"))
          (expect (buffer-string)
                  :to-equal "HEAD\nName: Bob   \nHello, Bob!\nTAIL")))))

  (it "leaves a field with no on-submit as it is"
    (let ((vui-render-delay nil))
      (save-window-excursion
        (with-temp-buffer
          (insert "HEAD\n\nTAIL")
          (vui-mount-inline (vui-component 'vui-fet-name) 6)
          (vui-fet--type-name)
          (expect (vui-fet--text 'name) :to-equal "Bob   ")
          (expect (buffer-string)
                  :to-equal "HEAD\nName: Bob   \nHello, Bob!\nTAIL")))))

  (it "submits the field it ends, not the one that starts there"
    ;; Text typed there goes into the field that ends there
    (let ((vui-render-delay nil))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (vui-mount-inline (vui-component 'vui-fet-submit-pair))
          (goto-char (widget-field-start (vui-fet--field 'a)))
          (execute-kbd-macro (kbd "C-e"))
          (expect (point) :to-equal (widget-field-start (vui-fet--field 'b)))
          (execute-kbd-macro (kbd "RET"))
          (expect vui-fet--submitted :to-equal '((a "aaa")))))))

  (dolist (value '("ab" ""))
    (it (format "submits a field followed by text on its line (%S)" value)
      ;; An empty field starts where it ends
      (let ((vui-render-delay nil))
        (save-window-excursion
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (vui-mount-inline (vui-component 'vui-fet-submit-row :value value))
            (goto-char (widget-field-end (vui-fet--field 'row)))
            (let ((text (buffer-string)))
              (execute-kbd-macro (kbd "RET"))
              (expect vui-fet--submitted :to-equal (list value))
              (expect (buffer-string) :to-equal text))))))))

(provide 'vui-field-edge-test)
;;; vui-field-edge-test.el ends here

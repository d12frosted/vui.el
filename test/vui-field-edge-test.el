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

(vui-defcomponent vui-fet-echo (size)
  "An empty field of SIZE that follows what is typed, and the text under it."
  :state ((text ""))
  :render (vui-vstack
           (vui-field :key 'echo :value text :size size
                      :on-change (lambda (value) (vui-set-state :text value)))
           (vui-text (format "echo %s" text))))

(vui-defcomponent vui-fet-last-field (label size value)
  "LABEL, when non-nil, then a field of SIZE with VALUE that ends the region."
  :render (vui-fragment (when label (vui-text label))
                        (vui-field :key 'last :value value :size size)))

(vui-defcomponent vui-fet-bare-stream (stream)
  "STREAM and nothing else."
  :render (vui-stream stream))

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
        (expect (buffer-string) :to-equal "HEAD\n\nTAIL"))))

  ;; All of the text goes: the end of the region is at the start of the
  ;; field, and the padding goes in after it
  (dolist (spec '(("L:" 1 "x") ("L:" 3 "abc") (nil 1 "x") (nil 3 "abc")))
    (pcase-let ((`(,label ,size ,value) spec))
      (it (format "keeps its padding inside the region when all its text goes (%S %d)"
                  label size)
        (with-temp-buffer
          (insert "HEAD\n\nTAIL")
          (let ((instance (vui-mount-inline
                           (vui-component 'vui-fet-last-field
                                          :label label :size size :value value)
                           6))
                (field (vui-fet--field 'last)))
            (delete-region (widget-field-start field) (widget-field-end field))
            (expect (buffer-string)
                    :to-equal (format "HEAD\n%s%s\nTAIL" (or label "")
                                      (make-string size ?\s)))
            (vui-unmount instance)
            (expect (buffer-string) :to-equal "HEAD\n\nTAIL"))))))

  (it "keeps the region around it when all the text of a stream row's field goes"
    ;; The row ends the stream, and the stream ends the inline mount
    (with-temp-buffer
      (insert "HEAD\n\nTAIL")
      (let* ((s (vui-make-stream))
             (instance (vui-mount-inline
                        (vui-component 'vui-fet-bare-stream :stream s) 6)))
        (vui-stream-append s (vui-component 'vui-fet-last-field
                                            :size 2 :value "ab"))
        (let ((field (vui-fet--field 'last)))
          (delete-region (widget-field-start field) (widget-field-end field)))
        (expect (buffer-string) :to-equal "HEAD\n  \nTAIL")
        (expect (vui--inline-bounds instance) :to-equal '(6 . 8))
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
          (expect (vui-fet--text 'word) :to-equal "Zabcd")))))

  ;; The field was empty and the text fills it: the start of the region
  ;; advances past all of the field, to its end
  (dolist (spec '((1 "x") (2 "xy")))
    (pcase-let ((`(,size ,text) spec))
      (it (format "keeps text that fills it inside the region (size %d)" size)
        (let ((vui-render-delay nil))
          (with-temp-buffer
            (insert "HEAD\n\nTAIL")
            (let ((instance (vui-mount-inline
                             (vui-component 'vui-fet-echo :size size) 6)))
              (goto-char (widget-field-start (vui-fet--field 'echo)))
              (insert text)
              (expect (buffer-string)
                      :to-equal (format "HEAD\n%s\necho %s\nTAIL" text text))
              (expect (vui-fet--text 'echo) :to-equal text)
              (expect (car (vui--inline-bounds instance)) :to-equal 6)
              (vui-update instance (list :size size))
              (expect (buffer-string)
                      :to-equal (format "HEAD\n%s\necho %s\nTAIL" text text))))))))

  (it "keeps the region around it when it starts a stream row"
    ;; The row starts the stream, and the stream starts the inline mount:
    ;; both regions start at the field
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (insert "HEAD\n\nTAIL")
        (let* ((s (vui-make-stream))
               (instance (vui-mount-inline
                          (vui-component 'vui-fet-chat :stream s :label "a") 6)))
          (vui-stream-append s (vui-component 'vui-fet-last-field
                                              :size 1 :value ""))
          (goto-char (widget-field-start (vui-fet--field 'last)))
          (insert "z")
          (expect (buffer-string) :to-equal "HEAD\nz\nstatus: a\nTAIL")
          (expect (car (vui--inline-bounds instance)) :to-equal 6)
          (vui-unmount instance)
          (expect (buffer-string) :to-equal "HEAD\n\nTAIL"))))))

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

(vui-defcomponent vui-fet-composer (size)
  "A field of SIZE that its `on-submit' clears, and the last sent text."
  :state ((text "") (sent nil))
  :render (vui-vstack
           (vui-field :key 'input :value text :size size
                      :on-change (lambda (value) (vui-set-state :text value))
                      :on-submit (lambda (value)
                                   (vui-batch
                                    (vui-set-state :sent value)
                                    (vui-set-state :text ""))))
           (vui-text (format "Sent: %s" sent))))

(vui-defcomponent vui-fet-shout ()
  "A field that upcases what is typed."
  :state ((text ""))
  :render (vui-field :key 'shout :value text :size 12
                     :on-change (lambda (value)
                                  (vui-set-state :text (upcase value)))))

(defun vui-fet--keys (keys &optional instance)
  "Run KEYS as if typed, then wait for the render INSTANCE scheduled."
  (execute-kbd-macro (kbd keys))
  (when instance (vui-fet--settle instance)))

(describe "a field whose text a render replaces"
  (dolist (delay '(nil 0.01))
    (it (format "puts point at the end of the new text (delay %s)" delay)
      (let ((vui-render-delay delay))
        (save-window-excursion
          (unwind-protect
              (let ((instance (vui-mount (vui-component 'vui-fet-composer
                                                        :size 10)
                                         "*vui-fet-chat*")))
                (with-current-buffer "*vui-fet-chat*"
                  (switch-to-buffer (current-buffer))
                  (goto-char (widget-field-start (vui-fet--field 'input)))
                  (vui-fet--keys "h i" instance)
                  (vui-fet--keys "RET" instance)
                  (expect (point)
                          :to-equal (widget-field-start (vui-fet--field 'input)))
                  (vui-fet--keys "y o" instance)
                  (expect (vui-fet--text 'input) :to-equal "yo        ")
                  (expect (buffer-substring-no-properties (point-min) (point-max))
                          :to-equal "yo        \nSent: hi")))
            (vui-fet--kill "*vui-fet-chat*"))))))

  (it "puts point at the end of the new text after a full field"
    ;; Point is at the end of the field, past its last character
    (let ((vui-render-delay nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (vui-mount (vui-component 'vui-fet-composer :size 3)
                         "*vui-fet-chat*")
              (with-current-buffer "*vui-fet-chat*"
                (switch-to-buffer (current-buffer))
                (goto-char (widget-field-start (vui-fet--field 'input)))
                (vui-fet--keys "a b c RET")
                (expect (point)
                        :to-equal (widget-field-start (vui-fet--field 'input)))
                (vui-fet--keys "x")
                (expect (vui-fet--text 'input) :to-equal "x  ")))
          (vui-fet--kill "*vui-fet-chat*")))))

  (it "puts point at the end of the new text in an inline region"
    (let ((vui-render-delay nil))
      (save-window-excursion
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert "HEAD\n\nTAIL")
          (vui-mount-inline (vui-component 'vui-fet-composer :size 4) 6)
          (goto-char (widget-field-start (vui-fet--field 'input)))
          (vui-fet--keys "h i RET y o")
          (expect (buffer-string)
                  :to-equal "HEAD\nyo  \nSent: hi\nTAIL")))))

  (it "keeps a space typed between words"
    ;; The value drops the trailing space, so the render writes the same
    ;; text as before: point stays after the space
    (let ((vui-render-delay nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (vui-mount (vui-component 'vui-fet-composer :size 10)
                         "*vui-fet-chat*")
              (with-current-buffer "*vui-fet-chat*"
                (switch-to-buffer (current-buffer))
                (goto-char (widget-field-start (vui-fet--field 'input)))
                (vui-fet--keys "a SPC b SPC SPC c")
                (expect (vui-fet--text 'input) :to-equal "a b  c    ")))
          (vui-fet--kill "*vui-fet-chat*")))))

  (dolist (delay '(nil 0.01))
    (it (format "keeps a space that came with new text (delay %s)" delay)
      ;; The render upcases the text, and the space after it is dropped
      ;; from the value: point stays after the space
      (let ((vui-render-delay delay))
        (save-window-excursion
          (unwind-protect
              (let ((instance (vui-mount (vui-component 'vui-fet-shout)
                                         "*vui-fet-shout*")))
                (with-current-buffer "*vui-fet-shout*"
                  (switch-to-buffer (current-buffer))
                  (goto-char (widget-field-start (vui-fet--field 'shout)))
                  (vui-fet--keys "h i SPC" instance)
                  (vui-fet--keys "x" instance)
                  (expect (vui-fet--text 'shout) :to-equal "HI X        ")))
            (vui-fet--kill "*vui-fet-shout*")))))))

(vui-defcomponent vui-fet-narrow (value)
  "A field of size 0 with VALUE, typed into, and text under it."
  :state ((n 0))
  :render (vui-vstack
           (vui-field :key 'narrow :value value :size 0
                      :on-change (lambda (_) (vui-set-state :n (1+ n))))
           (vui-text (format "tail %d" n))))

(describe "a field of size 0"
  (it "keeps a place to type at when it is empty"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (vui-mount-inline (vui-component 'vui-fet-narrow :value ""))
        (expect (buffer-string) :to-equal " \ntail 0")
        (expect (vui-fet--text 'narrow) :to-equal " ")
        (expect (widget-value (vui-fet--field 'narrow)) :to-equal ""))))

  (it "does not take in the text after it when it re-renders"
    ;; The value stays empty, so the render clears what was typed
    (let ((vui-render-delay nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (vui-mount (vui-component 'vui-fet-narrow :value "")
                         "*vui-fet-narrow*")
              (with-current-buffer "*vui-fet-narrow*"
                (switch-to-buffer (current-buffer))
                (goto-char (widget-field-start (vui-fet--field 'narrow)))
                (vui-fet--keys "x")
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal " \ntail 1")
                (expect (widget-value (vui-fet--field 'narrow)) :to-equal "")))
          (vui-fet--kill "*vui-fet-narrow*")))))

  (it "is as wide as its value"
    (let ((vui-render-delay nil))
      (with-temp-buffer
        (vui-mount-inline (vui-component 'vui-fet-narrow :value "ab"))
        (expect (buffer-string) :to-equal "ab\ntail 0")
        (expect (vui-fet--text 'narrow) :to-equal "ab")))))

(provide 'vui-field-edge-test)
;;; vui-field-edge-test.el ends here

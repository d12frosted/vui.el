;;; vui-stream-fields-test.el --- Editable fields in vui-stream items -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A stream writes its items in place, outside of any render: an append,
;; a node opened or grown, the last item or a node written again.  A
;; field in such an item is set up as a render sets up its fields, so it
;; can be edited, and the field of an item that is written again or
;; removed is forgotten with its text.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'vui-test-helpers)
(require 'cl-lib)

(vui-defcomponent vui-sf-chat (stream)
  "STREAM between two lines of text."
  :render (vui-vstack (vui-text "top")
                      (vui-stream stream)
                      (vui-text "bottom")))

(defvar vui-sf--changes nil
  "Values the fields of `vui-sf--item' reported, newest first.")

(defun vui-sf--item (label &optional value placeholder)
  "Return an item: LABEL, then a field holding VALUE (default \"hello\").
The field shows PLACEHOLDER while empty, and records its changes in
`vui-sf--changes'."
  (vui-fragment (vui-text (format "%s: " label))
                (vui-field :value (or value "hello") :size 8
                           :placeholder placeholder
                           :on-change (lambda (v) (push v vui-sf--changes)))))

(defmacro vui-sf--with-chat (&rest body)
  "Run BODY in a buffer with `vui-sf-chat' mounted.
STREAM is bound to its stream.  The stream holds one text item."
  (declare (indent 0))
  `(let* ((vui-render-delay nil)
          (vui-sf--changes nil)
          (stream (vui-make-stream))
          (instance (vui-mount (vui-component 'vui-sf-chat :stream stream)
                               "*vui-sf*")))
     (unwind-protect
         (with-current-buffer "*vui-sf*"
           (vui-stream-append stream (vui-text "first"))
           ,@body)
       (vui-unmount instance)
       (kill-buffer "*vui-sf*"))))

(defun vui-sf--goto (string)
  "Move point right after the first STRING in the buffer."
  (goto-char (point-min))
  (search-forward string))

(defun vui-sf--fields ()
  "Return (START END VALUE) of each field in `widget-field-list'."
  (mapcar (lambda (w)
            (list (widget-field-start w) (widget-field-end w)
                  (widget-value w)))
          (sort (copy-sequence widget-field-list)
                (lambda (a b) (< (widget-field-start a)
                                 (widget-field-start b))))))

(describe "a field in a stream item"
  (it "can be edited in an item appended in place"
    (vui-sf--with-chat
      (vui-stream-append stream (vui-sf--item "q"))
      (vui-sf--goto "hel")
      (insert "X")
      (expect (widget-value (widget-field-at (point))) :to-equal "helXlo")
      (expect (car vui-sf--changes) :to-equal "helXlo")))

  (it "can be edited in a node opened in place"
    (vui-sf--with-chat
      (vui-stream-open stream (vui-sf--item "q"))
      (vui-sf--goto "hel")
      (insert "X")
      (expect (widget-value (widget-field-at (point))) :to-equal "helXlo")))

  (it "can be edited when it grows a node"
    (vui-sf--with-chat
      (let ((node (vui-stream-open stream (vui-text "q: "))))
        (vui-stream-append-to node (vui-field :value "hello" :size 8))
        (vui-sf--goto "hel")
        (insert "X")
        (expect (widget-value (widget-field-at (point)))
                :to-equal "helXlo"))))

  (it "shows its placeholder while empty"
    (vui-sf--with-chat
      (vui-stream-append stream (vui-sf--item "q" "" "type here"))
      (vui-sf--goto "q: ")
      (expect (seq-find (lambda (ov) (overlay-get ov 'vui-placeholder))
                        (overlays-at (point)))
              :to-be-truthy)))

  (it "shows the placeholder of each field one write makes"
    (vui-sf--with-chat
      (vui-stream-append stream (vui-fragment (vui-sf--item "a" "" "one")
                                              (vui-sf--item "b" "" "two")))
      (expect (length (seq-filter
                       (lambda (ov) (overlay-get ov 'vui-placeholder))
                       (overlays-in (point-min) (point-max))))
              :to-equal 2)))

  (it "goes with the text of an error boundary that catches"
    ;; A hook appends the item while the boundary renders, and a later
    ;; child signals: the boundary deletes what it wrote, and shows its
    ;; fallback in its place.  The field the stream set up goes too, and
    ;; does not take in the fallback.
    (let* ((vui-render-delay nil)
           (vui-lifecycle-error-handler 'ignore)
           (stream (vui-make-stream))
           (instance nil))
      (vui-defcomponent vui-sf-appender ()
        :on-mount (vui-stream-append
                   stream (vui-fragment (vui-text "i:")
                                        (vui-field :value "G" :size 6)))
        :render (vui-text "C"))
      (vui-defcomponent vui-sf-boom ()
        :render (error "Boom"))
      (vui-defcomponent vui-sf-guarded ()
        :render (vui-vstack
                 (vui-text "top")
                 (vui-error-boundary
                  :fallback (lambda (_) (vui-text "FALLBACK"))
                  :children (list (vui-stream stream)
                                  (vui-component 'vui-sf-appender)
                                  (vui-component 'vui-sf-boom)))
                 (vui-text "bottom")))
      (vui-stream-append stream (vui-text "first"))
      (unwind-protect
          (progn
            (setq instance (vui-mount (vui-component 'vui-sf-guarded)
                                      "*vui-sf*"))
            (with-current-buffer "*vui-sf*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "top\nFALLBACK\nbottom")
              (expect widget-field-list :to-equal nil)
              (expect (seq-filter (lambda (ov) (overlay-get ov 'field))
                                  (overlays-in (point-min) (point-max)))
                      :to-equal nil)))
        (when instance (vui-unmount instance))
        (kill-buffer "*vui-sf*"))))

  (it "keeps a field next to the stream as it is"
    ;; The field of the last item ends where the next item is written
    (vui-sf--with-chat
      (vui-stream-append stream (vui-sf--item "a"))
      (vui-stream-append stream (vui-sf--item "b" "world"))
      (expect (mapcar #'cl-caddr (vui-sf--fields))
              :to-equal '("hello" "world"))
      (vui-sf--goto "hel")
      (insert "X")
      (expect (mapcar #'cl-caddr (vui-sf--fields))
              :to-equal '("helXlo" "world"))))

  (dolist (how '(update-last update))
    (describe (format "written again with `vui-stream-%s'" how)
      (let ((write (lambda (stream node vnode)
                     (if (eq how 'update-last)
                         (vui-stream-update-last stream vnode)
                       (vui-stream-update node vnode))))
            (open (lambda (stream vnode)
                    (if (eq how 'update-last)
                        (progn (vui-stream-append stream vnode) nil)
                      (prog1 (vui-stream-open stream vnode)
                        (vui-stream-append stream (vui-text "last")))))))

        (it "is replaced by the new item's field, which can be edited"
          (vui-sf--with-chat
            (let ((node (funcall open stream (vui-sf--item "q"))))
              (funcall write stream node (vui-sf--item "question"))
              (expect (length widget-field-list) :to-equal 1)
              (vui-sf--goto "question: hel")
              (insert "X")
              (expect (widget-value (widget-field-at (point)))
                      :to-equal "helXlo"))))

        (it "keeps point in the field"
          (vui-sf--with-chat
            (let ((node (funcall open stream (vui-sf--item "q"))))
              (vui-sf--goto "hel")
              (funcall write stream node (vui-sf--item "question"))
              ;; Not `line-beginning-position', which stops at the field
              (expect (buffer-substring-no-properties (pos-bol) (point))
                      :to-equal "question: hel")
              (expect (widget-field-at (point)) :to-be-truthy)))))))

  (dolist (side '(before after))
    (it (format "can be edited in a node put %s another" side)
      (vui-sf--with-chat
        (let ((node (vui-stream-open stream (vui-text "node"))))
          (funcall (if (eq side 'before) #'vui-stream-before #'vui-stream-after)
                   node (vui-sf--item "q"))
          (vui-sf--goto "hel")
          (insert "X")
          (expect (widget-value (widget-field-at (point)))
                  :to-equal "helXlo")))))

  (it "is forgotten with the oldest item when it is removed"
    ;; The separator after the item goes with it, not the one before
    (let* ((vui-render-delay nil)
           (stream (vui-make-stream))
           (instance (vui-mount (vui-component 'vui-sf-chat :stream stream)
                                "*vui-sf*")))
      (unwind-protect
          (with-current-buffer "*vui-sf*"
            (let ((node (vui-stream-open stream (vui-sf--item "q"))))
              (vui-stream-append stream (vui-sf--item "r" "world"))
              (vui-stream-remove node)
              (expect (mapcar #'cl-caddr (vui-sf--fields))
                      :to-equal '("world"))))
        (vui-unmount instance)
        (kill-buffer "*vui-sf*"))))

  (it "is forgotten with an item that is removed"
    (vui-sf--with-chat
      (let ((node (vui-stream-open stream (vui-sf--item "q"))))
        (vui-stream-append stream (vui-sf--item "r" "world"))
        (vui-stream-remove node)
        (expect (mapcar #'cl-caddr (vui-sf--fields)) :to-equal '("world"))
        (expect (seq-filter (lambda (ov) (overlay-get ov 'field))
                            (overlays-in (point-min) (point-max)))
                :to-have-same-items-as
                (list (widget-get (car widget-field-list)
                                  :field-overlay)))))))

;;; vui-stream-fields-test.el ends here

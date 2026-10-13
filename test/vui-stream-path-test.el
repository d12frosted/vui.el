;;; vui-stream-path-test.el --- Render paths of vui-stream items -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Every widget records its place in the tree as `:vui-path', and a
;; re-render finds the widget under point again by that path.  These
;; tests pin the paths in and around a `vui-stream':
;;
;;   AROUND - widgets below a live stream have the same path whether the
;;            render patched around the stream or rebuilt everything.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

;; The stream is the first child, so a re-render patches around it and
;; only rewrites the buttons below.
(vui-defcomponent vui-spath-tail (stream label)
  :render (vui-vstack (vui-stream stream)
                      (vui-hstack (vui-button "send")
                                  (vui-button (format "stop %s" label)))))

;; A fragment keeps its nil children, so the stream is child 1 here and
;; the button child 3.
(vui-defcomponent vui-spath-frag (stream label)
  :render (vui-fragment nil
                        (vui-stream stream)
                        (vui-newline)
                        (vui-button (format "stop %s" label))))

(defconst vui-spath--buffer "*vui-spath*")

(defun vui-spath--mount (type stream)
  "Mount component TYPE around STREAM and return the instance."
  (vui-mount (vui-component type :stream stream :label "a")
             vui-spath--buffer))

(defun vui-spath--kill ()
  (when (get-buffer vui-spath--buffer)
    (kill-buffer vui-spath--buffer)))

(defun vui-spath--paths ()
  "Return (LINE LABEL PATH) for every button of the buffer, in order.
LINE is the text of the button's line, which tells the items apart when
their buttons share a label."
  (with-current-buffer vui-spath--buffer
    (mapcar (lambda (button)
              (list (save-excursion
                      (goto-char (button-start button))
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
                    (vui--elt-get button :vui-tag)
                    (vui--elt-get button :vui-path)))
            (vui--collect-widgets))))

(describe "vui-stream paths: widgets below a live stream"
  (it "keeps their paths when the render patches around the stream"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-tail s)))
      (unwind-protect
          (let ((below '(("[send] [stop a]" "send" (1 0))
                         ("[send] [stop a]" "stop a" (1 1)))))
            ;; An empty stream is rendered like any other child.
            (expect (vui-spath--paths) :to-equal below)
            (vui-stream-append s (vui-text "m0"))
            (vui-stream-append s (vui-text "m1"))
            (expect (vui-spath--paths) :to-equal below)
            ;; From here on the stream is live and its text is left
            ;; alone: only the line below is rewritten.
            (with-current-buffer vui-spath--buffer
              (let ((probe (copy-marker 2)))
                (vui-update inst (list :stream s :label "b"))
                (expect (marker-position probe) :to-equal 2)
                (set-marker probe nil)))
            (expect (vui-spath--paths)
                    :to-equal '(("[send] [stop b]" "send" (1 0))
                                ("[send] [stop b]" "stop b" (1 1)))))
        (vui-spath--kill))))

  (it "counts the children before the stream too"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (inst (vui-spath--mount 'vui-spath-frag s)))
      (unwind-protect
          (progn
            (expect (vui-spath--paths) :to-equal '(("[stop a]" "stop a" (3))))
            (vui-stream-append s (vui-text "m0"))
            (vui-stream-append s (vui-text "m1"))
            (with-current-buffer vui-spath--buffer
              (let ((probe (copy-marker 2)))
                (vui-update inst (list :stream s :label "b"))
                (expect (buffer-string) :to-equal "m0\nm1\n[stop b]")
                (expect (marker-position probe) :to-equal 2)
                (set-marker probe nil)))
            (expect (vui-spath--paths) :to-equal '(("[stop b]" "stop b" (3)))))
        (vui-spath--kill))))

  (it "keeps the caught error of a boundary below the stream"
    ;; A boundary without an :id is told apart by its place in the tree.
    ;; When that place changed with the first patched render, the error
    ;; was forgotten and the failing child ran once more.
    (let ((vui-render-delay nil)
          (s (vui-make-stream))
          (caught 0))
      (vui-defcomponent vui-spath-broken ()
        :render (error "Boom"))
      (vui-defcomponent vui-spath-guarded (stream label)
        :render (vui-vstack
                 (vui-stream stream)
                 (vui-error-boundary
                  :fallback (lambda (_error)
                              (vui-text (format "failed %s" label)))
                  :on-error (lambda (_error) (cl-incf caught))
                  :children (list (vui-component 'vui-spath-broken)))))
      (let ((inst (vui-spath--mount 'vui-spath-guarded s)))
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "m0"))
              (vui-stream-append s (vui-text "m1"))
              (expect caught :to-equal 1)
              (with-current-buffer vui-spath--buffer
                (let ((probe (copy-marker 2)))
                  (vui-update inst (list :stream s :label "b"))
                  (expect (buffer-string) :to-equal "m0\nm1\nfailed b")
                  ;; The render patched around the stream.
                  (expect (marker-position probe) :to-equal 2)
                  (set-marker probe nil)))
              (expect caught :to-equal 1))
          (vui-spath--kill))))))

(provide 'vui-stream-path-test)
;;; vui-stream-path-test.el ends here

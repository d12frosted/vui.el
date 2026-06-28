;;; vui-stream-test.el --- vui-stream imperative append invariants -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Phase 1 invariants for `vui-stream' (issue #82, dir 5): an append-only
;; region that grows ABOVE a persistent declarative line.  The point of
;; the primitive is an O(1) imperative append, but the bar is correctness:
;;
;;   - after any sequence of appends the buffer is BYTE-IDENTICAL to what a
;;     plain list of the same items would render (the oracle);
;;   - the declarative content BELOW the stream stays correct as the stream
;;     grows (it just shifts down);
;;   - a full re-render (a sibling's state change) re-emits the items, so
;;     the wholesale path matches the oracle too;
;;   - point below the stream is preserved across an append above it.

;;; Code:

(require 'buttercup)
(require 'vui)

;; A stream above a declarative note line.  The handle is passed in as a
;; prop so the test owns it and can append imperatively.
(vui-defcomponent vui-st-app (stream note)
  :render
  (vui-vstack
   (vui-stream stream)
   (vui-text (format "note: %s" (or note "")))))

(defun vui-st--oracle (lines note)
  "Expected buffer string: LINES stacked above the NOTE line.
With no lines the stream is an empty vstack child and is dropped (no
leading separator), matching what a plain declarative list renders."
  (if lines
      (concat (mapconcat #'identity lines "\n") "\nnote: " note)
    (concat "note: " note)))

(defun vui-st--mount (stream note)
  (vui-mount (vui-component 'vui-st-app :stream stream :note note) "*vui-st*"))

(defun vui-st--buffer ()
  (with-current-buffer "*vui-st*" (buffer-string)))

(defun vui-st--kill ()
  (when (get-buffer "*vui-st*") (kill-buffer "*vui-st*")))

(describe "vui-stream: append is byte-identical to the oracle"
  (it "matches a plain list after a sequence of appends"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "line 1"))
              (vui-stream-append s (vui-text "line 2"))
              (vui-stream-append s (vui-text "line 3"))
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("line 1" "line 2" "line 3") "n0")))
          (vui-st--kill)))))

  (it "handles the empty stream and the first append (no leading newline)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "hi")))
        (ignore inst)
        (unwind-protect
            (progn
              ;; nothing appended yet: just the note, below an empty region
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '() "hi"))
              (vui-stream-append s (vui-text "first"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("first") "hi")))
          (vui-st--kill))))))

(describe "vui-stream: the line below stays correct as the stream grows"
  (it "keeps the note intact and in place across 100 appends"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "footer")))
        (ignore inst)
        (unwind-protect
            (let (lines)
              (dotimes (i 100)
                (let ((txt (format "msg %d" i)))
                  (push txt lines)
                  (vui-stream-append s (vui-text txt))))
              (setq lines (nreverse lines))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle lines "footer"))
              ;; the note line is still present, exactly once, at the bottom
              (with-current-buffer "*vui-st*"
                (goto-char (point-min))
                (expect (how-many "^note: footer$") :to-equal 1)
                (goto-char (point-max))
                (expect (line-number-at-pos)
                        :to-equal (1+ (length lines)))))
          (vui-st--kill))))))

(describe "vui-stream: a full re-render re-emits identically"
  (it "matches the oracle after a sibling state change rebuilds the buffer"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-text "b"))
              ;; change the note prop: forces a wholesale root re-render,
              ;; which must re-emit the stream items from the handle
              (vui-update inst (list :stream s :note "n1"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("a" "b") "n1"))
              ;; appends after a re-render keep working
              (vui-stream-append s (vui-text "c"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("a" "b" "c") "n1")))
          (vui-st--kill))))))

(describe "vui-stream: point below the stream is preserved on append"
  (it "leaves point in the note line after an append above it"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "tail")))
        (ignore inst)
        (unwind-protect
            (with-current-buffer "*vui-st*"
              (vui-stream-append s (vui-text "x"))
              ;; put point on the 'l' of "tail"
              (goto-char (point-max))
              (let ((before (char-before)))
                (vui-stream-append s (vui-text "y"))
                ;; point still at end of buffer, still after the same char
                (expect (point) :to-equal (point-max))
                (expect (char-before) :to-equal before)))
          (vui-st--kill))))))

(describe "vui-stream: update-last grows the in-progress item in place"
  (it "matches the oracle after updating the last item (token streaming)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "line 1"))
              (vui-stream-append s (vui-text "par"))      ; in-progress msg
              ;; tokens arrive: re-render the last item with the message so far
              (vui-stream-update-last s (vui-text "partial"))
              (vui-stream-update-last s (vui-text "partial response"))
              (vui-stream-update-last s (vui-text "partial response done"))
              (expect (vui-st--buffer)
                      :to-equal
                      (vui-st--oracle '("line 1" "partial response done") "n0")))
          (vui-st--kill)))))

  (it "works on the very first item (which took the re-render path)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "hi")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "first"))    ; empty -> non-empty
              (vui-stream-update-last s (vui-text "first updated"))
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("first updated") "hi")))
          (vui-st--kill)))))

  (it "appends correctly after an update-last (continue the stream)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "z")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "a"))
              (vui-stream-update-last s (vui-text "aa"))
              (vui-stream-append s (vui-text "b"))
              (vui-stream-update-last s (vui-text "bb"))
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("aa" "bb") "z")))
          (vui-st--kill)))))

  (it "leaves the line below correct after an update-last"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "footer")))
        (ignore inst)
        (unwind-protect
            (progn
              (dotimes (i 50) (vui-stream-append s (vui-text (format "m%d" i))))
              (vui-stream-update-last s (vui-text "m49 GROWN with more text"))
              (with-current-buffer "*vui-st*"
                (goto-char (point-min))
                (expect (how-many "^note: footer$") :to-equal 1)
                (goto-char (point-max))
                (expect (char-before) :to-equal ?r)))  ; ...footer
          (vui-st--kill)))))

  (it "is a no-op on an empty stream"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-update-last s (vui-text "ignored"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '() "x")))
          (vui-st--kill))))))

(describe "vui-stream: update-last appends only the delta when text grows"
  (it "grows the last text item incrementally, matching a full render"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "line 1"))
              (vui-stream-append s (vui-text "Hel"))         ; in-progress msg
              (vui-stream-update-last s (vui-text "Hello"))         ; extend
              (vui-stream-update-last s (vui-text "Hello there")) ; extend more
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("line 1" "Hello there") "n0")))
          (vui-st--kill)))))

  (it "falls back to a full re-render when the new text is not an extension"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "Hello world"))
              (vui-stream-update-last s (vui-text "Goodbye"))   ; not a prefix
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("Goodbye") "n0")))
          (vui-st--kill))))))

(describe "vui-stream: first append after the empty stream was re-rendered"
  ;; Regression: a re-render of the still-EMPTY stream (e.g. a sibling box
  ;; state change) used to record the stream as stream-tail; the first
  ;; append then re-rendered via the stream-tail patch, which leaves the
  ;; (empty) region untouched, so the first item was never emitted.
  (it "still renders the first item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "n0")))
        (unwind-protect
            (progn
              ;; re-render while the stream is still empty
              (vui-update inst (list :stream s :note "n1"))
              ;; the very first append must actually appear
              (vui-stream-append s (vui-text "first"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("first") "n1")))
          (vui-st--kill))))))

(provide 'vui-stream-test)
;;; vui-stream-test.el ends here

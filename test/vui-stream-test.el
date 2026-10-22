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

  (it "re-renders when the grown text's prefix changed only in properties"
    ;; The extend fast path appends just the new suffix, but
    ;; `string-prefix-p' compares characters only: when the old prefix's
    ;; text properties changed too, appending the suffix would leave the
    ;; prefix's stale properties in the buffer.
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "p")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text (propertize "load" 'face 'shadow)))
              (vui-stream-update-last
               s (vui-text (propertize "loaded" 'face 'success)))
              (with-current-buffer "*vui-st*"
                (expect (get-text-property 1 'face) :to-equal 'success))
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("loaded") "p")))
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

(describe "vui--string-prefix-equal-including-properties-p"
  ;; The allocation-free replacement for
  ;; (equal-including-properties O (substring N 0 (length O))): must give
  ;; the same verdict without copying the prefix.
  (it "accepts a plain equal prefix"
    (expect (vui--string-prefix-equal-including-properties-p "abc" "abcdef")
            :to-be-truthy))

  (it "accepts an equal prefix with matching properties"
    (expect (vui--string-prefix-equal-including-properties-p
             (propertize "ab" 'face 'bold)
             (concat (propertize "ab" 'face 'bold) "cd"))
            :to-be-truthy))

  (it "rejects when characters differ inside the prefix"
    (expect (vui--string-prefix-equal-including-properties-p "abx" "abcdef")
            :not :to-be-truthy))

  (it "rejects when old is longer than new"
    (expect (vui--string-prefix-equal-including-properties-p "abcdef" "abc")
            :not :to-be-truthy))

  (it "rejects when a property differs inside the prefix"
    (expect (vui--string-prefix-equal-including-properties-p
             (propertize "ab" 'face 'bold)
             (concat (propertize "ab" 'face 'shadow) "cd"))
            :not :to-be-truthy))

  (it "rejects when only the new string has properties in the prefix"
    (expect (vui--string-prefix-equal-including-properties-p
             "ab" (propertize "abcd" 'face 'bold))
            :not :to-be-truthy))

  (it "rejects when a property run boundary moved inside the prefix"
    ;; old: [a]bold [b]plain; new: [ab]bold - same chars, same property
    ;; lists at 0, but the bold run covers one more char of the prefix.
    (expect (vui--string-prefix-equal-including-properties-p
             (concat (propertize "a" 'face 'bold) "b")
             (concat (propertize "ab" 'face 'bold) "c"))
            :not :to-be-truthy))

  (it "ignores a property change exactly at the old length"
    ;; The new string starts a new run at (length old); that run lies
    ;; entirely outside the compared range and must not disqualify.
    (expect (vui--string-prefix-equal-including-properties-p
             (propertize "ab" 'face 'bold)
             (concat (propertize "ab" 'face 'bold)
                     (propertize "cd" 'face 'shadow)))
            :to-be-truthy))

  (it "accepts an empty old string"
    (expect (vui--string-prefix-equal-including-properties-p
             "" (propertize "ab" 'face 'bold))
            :to-be-truthy))

  (it "handles multiple property intervals in the old string"
    (let ((o (concat (propertize "a" 'face 'bold)
                     "b"
                     (propertize "c" 'face 'italic))))
      (expect (vui--string-prefix-equal-including-properties-p
               o (concat o "d"))
              :to-be-truthy)
      ;; same shape but the middle interval differs
      (expect (vui--string-prefix-equal-including-properties-p
               o (concat (propertize "a" 'face 'bold)
                         (propertize "b" 'face 'shadow)
                         (propertize "c" 'face 'italic)
                         "d"))
              :not :to-be-truthy))))

(describe "vui-stream: update-last extend fast path with propertized text"
  ;; These specs pin the MECHANISM, not just the result: a marker inside
  ;; the prefix survives the fast path (only the suffix is inserted at
  ;; the end) but collapses to the item start when the item is deleted
  ;; and re-rendered wholesale.
  (it "appends only the suffix when the propertized prefix is unchanged"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "p")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append
               s (vui-text (concat (propertize "bold" 'face 'bold) " tail")))
              (let ((m (with-current-buffer "*vui-st*" (copy-marker 3))))
                (vui-stream-update-last
                 s (vui-text (concat (propertize "bold" 'face 'bold)
                                     " tail grows")))
                (expect (marker-position m) :to-equal 3)
                (with-current-buffer "*vui-st*"
                  (expect (get-text-property 1 'face) :to-equal 'bold))
                (expect (vui-st--buffer)
                        :to-equal (vui-st--oracle '("bold tail grows") "p"))))
          (vui-st--kill)))))

  (it "takes the fast path when a property run starts exactly at the old length"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "p")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text (propertize "ab" 'face 'bold)))
              (let ((m (with-current-buffer "*vui-st*" (copy-marker 2))))
                (vui-stream-update-last
                 s (vui-text (concat (propertize "ab" 'face 'bold)
                                     (propertize "cd" 'face 'shadow))))
                (expect (marker-position m) :to-equal 2)
                (with-current-buffer "*vui-st*"
                  (expect (get-text-property 1 'face) :to-equal 'bold)
                  (expect (get-text-property 3 'face) :to-equal 'shadow))
                (expect (vui-st--buffer)
                        :to-equal (vui-st--oracle '("abcd") "p"))))
          (vui-st--kill)))))

  (it "takes the fast path over multiple property intervals in the old text"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "p"))
            (old (concat (propertize "a" 'face 'bold)
                         "b"
                         (propertize "c" 'face 'italic))))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text old))
              (let ((m (with-current-buffer "*vui-st*" (copy-marker 2))))
                (vui-stream-update-last s (vui-text (concat old "d")))
                (expect (marker-position m) :to-equal 2)
                (with-current-buffer "*vui-st*"
                  (expect (get-text-property 1 'face) :to-equal 'bold)
                  (expect (get-text-property 3 'face) :to-equal 'italic))
                (expect (vui-st--buffer)
                        :to-equal (vui-st--oracle '("abcd") "p"))))
          (vui-st--kill)))))

  (it "extends an item whose old text is empty"
    ;; The empty old text is a trivial prefix of anything: the whole new
    ;; text is the suffix.  (A SECOND item, so the update stays inside a
    ;; non-empty region and isolates the prefix check; the empty FIRST
    ;; item crosses the empty boundary and has its own specs below.)
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "line 1"))
              (vui-stream-append s (vui-text ""))
              (vui-stream-update-last s (vui-text "hi"))
              (expect (vui-st--buffer)
                      :to-equal (vui-st--oracle '("line 1" "hi") "x")))
          (vui-st--kill)))))

  (it "re-renders wholesale when a property changed inside the prefix"
    ;; Companion to the marker specs above: same shape, but the prefix's
    ;; properties changed, so the fast path must be REJECTED - the marker
    ;; collapses to the item start and no stale prefix survives.
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "p")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text (propertize "load" 'face 'shadow)))
              (let ((m (with-current-buffer "*vui-st*" (copy-marker 3))))
                (vui-stream-update-last
                 s (vui-text (propertize "loaded" 'face 'success)))
                (expect (marker-position m) :to-equal 1)
                (with-current-buffer "*vui-st*"
                  (expect (get-text-property 1 'face) :to-equal 'success))
                (expect (vui-st--buffer)
                        :to-equal (vui-st--oracle '("loaded") "p"))))
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

;; A root that skips its update commits the same tree again, which
;; writes nothing.  The first or the last item of a stream changes the
;; layout around it all the same, so the render it asks for has to write
;; the tree.

(vui-defcomponent vui-st-frozen (stream)
  :should-update nil
  :render (vui-vstack (vui-stream stream) (vui-text "footer")))

(describe "vui-stream: a root that skips its update"
  (it "shows the first item and lets go of the last one"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-mount (vui-component 'vui-st-frozen :stream s) "*vui-st-frozen*")
      (unwind-protect
          (with-current-buffer "*vui-st-frozen*"
            ;; The mount writes no record; from this render on there is one
            (vui-rerender (vui-get-instance))
            (let ((node (vui-stream-open s (vui-text "a"))))
              (expect (buffer-string) :to-equal "a\nfooter")
              ;; rewritten to nothing and back
              (vui-stream-update node (vui-text ""))
              (expect (buffer-string) :to-equal "footer")
              (vui-stream-update node (vui-text "b"))
              (expect (buffer-string) :to-equal "b\nfooter")
              (vui-stream-remove node)
              (expect (buffer-string) :to-equal "footer")
              (vui-stream-append s (vui-text "c"))
              (expect (buffer-string) :to-equal "c\nfooter")))
        (kill-buffer "*vui-st-frozen*"))))

  (it "shows a component appended to an empty stream"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-mount (vui-component 'vui-st-frozen :stream s) "*vui-st-frozen*")
      (unwind-protect
          (with-current-buffer "*vui-st-frozen*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append s (vui-component 'vui-st-frozen-row))
            (expect (buffer-string) :to-equal "[row]\nfooter"))
        (kill-buffer "*vui-st-frozen*")))))

(vui-defcomponent vui-st-frozen-row ()
  :render (vui-text "[row]"))

;; A root whose first child is a stream patches around it.  A second
;; stream below it changes the layout of its own part only, so its first
;; and last items must not make the first stream written again.  A
;; stream inside a component row of the first stream lives in that
;; stream's text, so for it the first stream has to be written again.

(defvar vui-st-tail--renders nil
  "Handles `vui--stream-render' was called with, latest first.")

(defun vui-st-tail--record (handle)
  (push handle vui-st-tail--renders))

(vui-defcomponent vui-st-tail-panel (stream)
  :render (vui-vstack (vui-text "panel") (vui-stream stream)))

(vui-defcomponent vui-st-tail-root (log panel)
  :render (vui-vstack (vui-stream log)
                      (vui-component 'vui-st-tail-panel :stream panel)))

;; A component whose only text is its stream: while that stream is
;; empty, nothing separates it from the first stream, so it starts right
;; where the first stream ends.
(vui-defcomponent vui-st-tail-bare (stream)
  :render (vui-stream stream))

(vui-defcomponent vui-st-tail-bare-root (log panel mode)
  :render (let ((kids (list (vui-stream log)
                            (vui-component 'vui-st-tail-bare :stream panel))))
            (if (eq mode 'fragment)
                (apply #'vui-fragment kids)
              (apply #'vui-vstack kids))))

(defvar vui-st-tail--inner nil
  "The stream inside `vui-st-tail-card'.")

(vui-defcomponent vui-st-tail-card ()
  :render (vui-vstack (vui-text "card")
                      (vui-stream vui-st-tail--inner)
                      (vui-text "end")))

(describe "vui-stream: a second stream below a first-child stream"
  (it "does not write the first stream again for its first and last items"
    (let ((vui-render-delay nil)
          (log (vui-make-stream))
          (panel (vui-make-stream)))
      (dotimes (i 3) (vui-stream-append log (vui-text (format "log%d" i))))
      (vui-mount (vui-component 'vui-st-tail-root :log log :panel panel)
                 "*vui-st-tail*")
      (advice-add 'vui--stream-render :before #'vui-st-tail--record)
      (unwind-protect
          (with-current-buffer "*vui-st-tail*"
            (vui-rerender (vui-get-instance))
            (setq vui-st-tail--renders nil)
            (let ((node (vui-stream-open panel (vui-text "p1"))))
              (expect (buffer-string) :to-equal "log0\nlog1\nlog2\npanel\np1")
              (vui-stream-remove node)
              (expect (buffer-string) :to-equal "log0\nlog1\nlog2\npanel"))
            (expect (cl-count log vui-st-tail--renders) :to-equal 0)
            (expect (cl-count panel vui-st-tail--renders) :to-equal 2))
        (advice-remove 'vui--stream-render #'vui-st-tail--record)
        (kill-buffer "*vui-st-tail*"))))

  (dolist (mode '(vstack fragment))
    (it (format "leaves the first stream alone for a stream right after it (%s)"
                mode)
      (let ((vui-render-delay nil)
            (log (vui-make-stream))
            (panel (vui-make-stream)))
        (dotimes (i 3) (vui-stream-append log (vui-text (format "log%d" i))))
        (vui-mount (vui-component 'vui-st-tail-bare-root
                     :log log :panel panel :mode mode)
                   "*vui-st-tail*")
        (advice-add 'vui--stream-render :before #'vui-st-tail--record)
        (unwind-protect
            (with-current-buffer "*vui-st-tail*"
              (vui-rerender (vui-get-instance))
              (setq vui-st-tail--renders nil)
              (let ((node (vui-stream-open panel (vui-text "p1"))))
                (expect (buffer-string)
                        :to-equal (if (eq mode 'fragment)
                                      "log0\nlog1\nlog2p1"
                                    "log0\nlog1\nlog2\np1"))
                (vui-stream-remove node)
                (expect (buffer-string) :to-equal "log0\nlog1\nlog2"))
              (expect (cl-count log vui-st-tail--renders) :to-equal 0))
          (advice-remove 'vui--stream-render #'vui-st-tail--record)
          (kill-buffer "*vui-st-tail*")))))

  (it "shows the first item of a stream inside a row of the first stream"
    (let ((vui-render-delay nil)
          (log (vui-make-stream)))
      (setq vui-st-tail--inner (vui-make-stream))
      (vui-stream-append log (vui-text "l0"))
      (vui-mount (vui-component 'vui-st-frozen :stream log) "*vui-st-tail*")
      (unwind-protect
          (with-current-buffer "*vui-st-tail*"
            (vui-rerender (vui-get-instance))
            (vui-stream-append log (vui-component 'vui-st-tail-card))
            (expect (buffer-string) :to-equal "l0\ncard\nend\nfooter")
            (vui-stream-append vui-st-tail--inner (vui-text "x"))
            (expect (buffer-string) :to-equal "l0\ncard\nx\nend\nfooter"))
        (kill-buffer "*vui-st-tail*")))))

(describe "vui-stream: update-last on an empty-rendering stream re-lays the tree"
  ;; Mirror of the empty -> non-empty append transition: when an in-place
  ;; update shrinks the WHOLE stream region to zero width, the separator
  ;; the container emitted around the stream is stale (a fresh mount drops
  ;; the zero-output child), so the tree must be re-laid once.
  (it "drops the stray separator when update-last empties the only item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "a"))
              (vui-stream-update-last s (vui-text ""))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '() "x")))
          (vui-st--kill)))))

  (it "drops the stray separator when vui-stream-update empties the only node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (let ((n (vui-stream-open s (vui-text "a"))))
              (vui-stream-update n (vui-text ""))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '() "x")))
          (vui-st--kill)))))

  (it "restores the separator when update-last refills the emptied item"
    ;; The other direction of the same boundary: once the emptied stream
    ;; was re-laid (separator dropped), refilling it must bring the
    ;; separator back, exactly like the first append on an empty stream.
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "a"))
              (vui-stream-update-last s (vui-text ""))
              (vui-stream-update-last s (vui-text "b"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("b") "x")))
          (vui-st--kill)))))

  (it "restores the separator when vui-stream-update refills the emptied node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (let ((n (vui-stream-open s (vui-text "a"))))
              (vui-stream-update n (vui-text ""))
              (vui-stream-update n (vui-text "b"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("b") "x")))
          (vui-st--kill)))))

  (it "restores the separator when append-to grows the emptied node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (let ((n (vui-stream-open s (vui-text "a"))))
              (vui-stream-update n (vui-text ""))
              (vui-stream-append-to n (vui-text "b"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("b") "x")))
          (vui-st--kill)))))

  (it "keeps the in-place patch when other items still render"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-text "b"))
              (vui-stream-update-last s (vui-text ""))
              ;; The stream still renders "a\n" (item + separator), so no
              ;; re-lay: the buffer matches the oracle for ("a" "").
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("a" "") "x")))
          (vui-st--kill))))))

(describe "vui-stream: update-last grows an empty FIRST item"
  ;; A different way into the zero-width region than emptying an existing
  ;; item: an empty text appended as the FIRST item renders zero
  ;; characters, so the container drops the stream child at the empty ->
  ;; non-empty re-render and the separator to the content below is never
  ;; emitted at all.  Growing the item via update-last must re-lay the
  ;; tree, on both of its paths, or the item comes out glued to the note
  ;; ("hinote: x").
  (it "recovers the separator on the extend fast path"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text ""))
              ;; zero-char stream child is dropped, like a plain render
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '() "x"))
              ;; "" -> "hi" is an extension, so this hits the fast path
              (vui-stream-update-last s (vui-text "hi"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("hi") "x")))
          (vui-st--kill)))))

  (it "recovers the separator on the replace path"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text ""))
              ;; a different face defeats the extend fast path
              (vui-stream-update-last s (vui-text "hi" :face 'bold))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("hi") "x")))
          (vui-st--kill)))))

  (it "keeps appending correctly after the recovery"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((inst (vui-st--mount s "x")))
        (ignore inst)
        (unwind-protect
            (progn
              (vui-stream-append s (vui-text ""))
              (vui-stream-update-last s (vui-text "hi"))
              (vui-stream-append s (vui-text "b"))
              (expect (vui-st--buffer) :to-equal (vui-st--oracle '("hi" "b") "x")))
          (vui-st--kill))))))

;; A container drops a child that writes nothing, and the separator it
;; would have had.  So when the text of a stream goes empty, or comes
;; back, the separators around it change, whatever made it change: a
;; remove that leaves only items with no text, an append to such a
;; stream, or a component row that renders again on its own.

(vui-defcomponent vui-st-empty-row (label)
  :render (vui-text label))

(vui-defcomponent vui-st-empty-root (stream mode)
  :render (if (eq mode 'first)
              (vui-vstack (vui-stream stream) (vui-text "footer"))
            (vui-vstack (vui-text "x") (vui-stream stream) (vui-text "footer"))))

(defmacro vui-st-empty--with (mode &rest body)
  "Mount `vui-st-empty-root' in MODE with a stream S, run BODY there."
  (declare (indent 1))
  `(let ((vui-render-delay nil)
         (s (vui-make-stream)))
     (vui-mount (vui-component 'vui-st-empty-root :stream s :mode ,mode)
                "*vui-st-empty*")
     (unwind-protect
         (with-current-buffer "*vui-st-empty*"
           (vui-rerender (vui-get-instance))
           ,@body)
       (kill-buffer "*vui-st-empty*"))))

(defun vui-st-empty--text ()
  (replace-regexp-in-string "\n" "|" (buffer-string)))

(describe "vui-stream: a stream whose text goes empty"
  (it "lets go of its separator when a remove leaves an item with no text"
    (vui-st-empty--with 'middle
      (let* ((node (vui-stream-open s (vui-text "a")))
             (other (vui-stream-open s (vui-text "b"))))
        (vui-stream-update other (vui-text ""))
        (vui-stream-remove node)
        (expect (vui-st-empty--text) :to-equal "x|footer")
        (vui-stream-append s (vui-text "c"))
        (expect (vui-st-empty--text) :to-equal "x||c|footer"))))

  (dolist (side '(before after))
    (it (format "gets its separator back for an item put %s one with no text"
                side)
      (vui-st-empty--with 'middle
        (let* ((node (vui-stream-open s (vui-text "a")))
               (empty (vui-stream-open s (vui-text ""))))
          (vui-stream-remove node)
          (expect (vui-st-empty--text) :to-equal "x|footer")
          (if (eq side 'before)
              (vui-stream-before empty (vui-text "z"))
            (vui-stream-after empty (vui-text "z")))
          (expect (vui-st-empty--text)
                  :to-equal (if (eq side 'before) "x|z||footer" "x||z|footer"))
          ;; both nodes stay live
          (vui-stream-update empty (vui-text "e"))
          (expect (vui-st-empty--text)
                  :to-equal (if (eq side 'before) "x|z|e|footer" "x|e|z|footer"))))))

  (it "mounts a component on a stream left with only a row with no text"
    (vui-st-empty--with 'middle
      (let ((node (vui-stream-open s (vui-text "a")))
            (card nil))
        (vui-stream-append s (vui-component 'vui-st-empty-row :label ""))
        (vui-stream-remove node)
        (setq card (vui-stream-open
                    s (vui-component 'vui-st-empty-row :label "c1")))
        (expect (vui-st-empty--text) :to-equal "x||c1|footer")
        ;; a row, which its node updates
        (vui-stream-update card (vui-component 'vui-st-empty-row :label "c2"))
        (expect (vui-st-empty--text) :to-equal "x||c2|footer"))))

  (it "lets go of its separator when a remove leaves a row with no text"
    (vui-st-empty--with 'middle
      (let ((node (vui-stream-open s (vui-text "a"))))
        (vui-stream-append s (vui-component 'vui-st-empty-row :label ""))
        (vui-stream-remove node)
        (expect (vui-st-empty--text) :to-equal "x|footer")
        (vui-stream-append s (vui-text "b"))
        (expect (vui-st-empty--text) :to-equal "x||b|footer"))))

  (it "follows a row that goes empty and back on its own"
    (vui-st-empty--with 'middle
      ;; A component opened on an empty stream is not a row, so the row
      ;; goes after an item that is removed next
      (let* ((node (vui-stream-open s (vui-text "a")))
             (row (vui-stream-open
                   s (vui-component 'vui-st-empty-row :label "r"))))
        (vui-stream-remove node)
        (expect (vui-st-empty--text) :to-equal "x|r|footer")
        (vui-stream-update row (vui-component 'vui-st-empty-row :label ""))
        (expect (vui-st-empty--text) :to-equal "x|footer")
        (vui-stream-update row (vui-component 'vui-st-empty-row :label "r2"))
        (expect (vui-st-empty--text) :to-equal "x|r2|footer"))))

  (it "keeps a row with no text through a render of the root"
    ;; The stream is the first child and holds only a row that renders
    ;; nothing: the render of the root writes the stream again, with the
    ;; row
    (vui-st-empty--with 'first
      (let* ((node (vui-stream-open s (vui-text "a")))
             (row (vui-stream-open
                   s (vui-component 'vui-st-empty-row :label ""))))
        (vui-stream-remove node)
        (expect (vui-st-empty--text) :to-equal "footer")
        (vui-rerender (vui-get-instance))
        (expect (vui-st-empty--text) :to-equal "footer")
        (vui-stream-update row (vui-component 'vui-st-empty-row :label "r"))
        (expect (vui-st-empty--text) :to-equal "r|footer")
        (vui-stream-append s (vui-text "b"))
        (expect (vui-st-empty--text) :to-equal "r|b|footer")))))

(defvar vui-st-hook-tail--stream nil
  "The stream of `vui-st-hook-tail-root'.")

(defvar vui-st-hook-tail--count 1
  "The number of the last item `vui-st-hook-tail-appender' appended.")

(vui-defcomponent vui-st-hook-tail-appender (text)
  "TEXT, or nothing when TEXT is nil.  Its `:on-update' appends an item
to `vui-st-hook-tail--stream'."
  :on-update (vui-stream-append
              vui-st-hook-tail--stream
              (vui-text (format "s%d" (cl-incf vui-st-hook-tail--count))))
  :render (and text (vui-text text)))

(vui-defcomponent vui-st-hook-tail-root (text)
  :render (vui-vstack (vui-stream vui-st-hook-tail--stream)
                      (vui-component 'vui-st-hook-tail-appender :text text)))

(describe "vui-stream: content after it whose hook appends to it"
  ;; The render of the root writes the content after the stream again:
  ;; the hook adds to the stream, above that content
  (it "drops the separator of the content when it writes nothing"
    (let ((vui-render-delay nil)
          (vui-st-hook-tail--stream (vui-make-stream))
          (vui-st-hook-tail--count 1))
      (vui-stream-append vui-st-hook-tail--stream (vui-text "s1"))
      (vui-mount (vui-component 'vui-st-hook-tail-root :text "c") "*vui-st-hook-tail*")
      (unwind-protect
          (with-current-buffer "*vui-st-hook-tail*"
            ;; The renders after this one patch around the stream
            (vui-rerender (vui-get-instance))
            (vui-update (vui-get-instance) '(:text "c2"))
            (expect (buffer-string) :to-equal "s1\ns2\ns3\nc2")
            (vui-update (vui-get-instance) '(:text nil))
            (expect (buffer-string) :to-equal "s1\ns2\ns3\ns4"))
        (kill-buffer "*vui-st-hook-tail*")))))

(defvar vui-st--at-point-stream nil
  "The stream of the trees in \"a hook that appends to a stream that ends
at point\" and the describes after it.")

(vui-defcomponent vui-st-silent-appender (fresh)
  "Render nothing; append \"added\" when mounted, unless FRESH."
  :on-mount (unless fresh
              (vui-stream-append vui-st--at-point-stream (vui-text "added")))
  :render (vui-fragment))

(vui-defcomponent vui-st-boom ()
  :render (error "Boom"))

(vui-defcomponent vui-st-at-point-tree (shape fresh)
  "The stream `vui-st--at-point-stream', then what SHAPE puts after it.
SHAPE is `alone' or `then-text' (a component that renders nothing and
appends to the stream when mounted, unless FRESH; then text \"FB\"), or
`on-error' (a boundary whose child signals and whose `:on-error'
appends, unless FRESH), or `faced' (a faced vstack that starts with that
component, then \"X\")."
  :render
  (pcase shape
    ('faced
     (vui-vstack
      (vui-fragment
       (vui-stream vui-st--at-point-stream)
       (vui-vstack :face 'bold
                   (vui-component 'vui-st-silent-appender :fresh fresh)
                   (vui-text "X")))
      (vui-text "bottom")))
    ('on-error
     (vui-vstack
      :indent 2
      (vui-fragment
       (vui-stream vui-st--at-point-stream)
       (vui-error-boundary
        :on-error (lambda (_)
                    (unless fresh
                      (vui-stream-append vui-st--at-point-stream
                                         (vui-text "added"))))
        :fallback (lambda (_) (vui-text "FB"))
        :children (list (vui-component 'vui-st-boom))))
      (vui-text "bottom")))
    (_
     (vui-vstack
      (apply #'vui-fragment
             (vui-stream vui-st--at-point-stream)
             (vui-component 'vui-st-silent-appender :fresh fresh)
             (when (eq shape 'then-text)
               (list (vui-text "FB"))))
      (vui-text "bottom")))))

(defun vui-st--fresh-text (shape items)
  "Return the text of the SHAPE tree rendered with its stream holding ITEMS.
No hook appends there: the oracle of what the tree looks like after its
hook appended to the stream (see `vui-st-at-point-tree')."
  (let ((vui-lifecycle-error-handler 'ignore)
        (vui-render-delay nil))
    (setq vui-st--at-point-stream (vui-make-stream))
    (dolist (item items)
      (vui-stream-append vui-st--at-point-stream (vui-text item)))
    (unwind-protect
        (let ((instance (vui-mount (vui-component 'vui-st-at-point-tree
                                                  :shape shape :fresh t)
                                   "*vui-st-at-point-fresh*")))
          (prog1 (with-current-buffer "*vui-st-at-point-fresh*"
                   (vui-st--faces))
            (vui-unmount instance)))
      (kill-buffer "*vui-st-at-point-fresh*")
      (setq vui-st--at-point-stream nil))))

(defun vui-st--faces ()
  "Return the text of the buffer with the runs of its `face' property.
\(TEXT . RUNS), RUNS a list of (FROM TO FACE) for each run with a face."
  (let ((runs nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'face nil (point-max)))
            (face (get-text-property pos 'face)))
        (when face
          (push (list pos next face) runs))
        (setq pos next)))
    (cons (buffer-substring-no-properties (point-min) (point-max))
          (nreverse runs))))

(describe "a hook that appends to a stream that ends at point"
  ;; The render is writing right where the stream ends.  The item is
  ;; the stream's, before what the render writes next, as in a render
  ;; of the tree with the item in the stream from the start.
  (after-each
    (setq vui-st--at-point-stream nil))

  (dolist (shape '(alone then-text on-error faced))
    (it (format "puts the item before what the render writes next (%s)" shape)
      (let* ((vui-render-delay nil)
             (vui-lifecycle-error-handler 'ignore)
             (expected (vui-st--fresh-text shape '("first" "added")))
             (later (vui-st--fresh-text shape '("first" "added" "later"))))
        (setq vui-st--at-point-stream (vui-make-stream))
        (vui-stream-append vui-st--at-point-stream (vui-text "first"))
        (let ((instance (vui-mount (vui-component 'vui-st-at-point-tree
                                                  :shape shape)
                                   "*vui-st-at-point*")))
          (unwind-protect
              (with-current-buffer "*vui-st-at-point*"
                ;; With the faces: a faced container that starts there
                ;; starts after the item
                (expect (vui-st--faces) :to-equal expected)
                ;; The stream ends after its item, before the rest
                (vui-stream-append vui-st--at-point-stream (vui-text "later"))
                (expect (vui-st--faces) :to-equal later))
            (vui-unmount instance)
            (kill-buffer "*vui-st-at-point*")))))))

(defvar vui-st--show-row nil
  "A function that makes `vui-st-growing-row' render its text.")

(vui-defcomponent vui-st-growing-row ()
  "Nothing at first; then A, a component that appends to the stream
when mounted (see `vui-st-silent-appender'), and B."
  :state ((show nil))
  :render (progn
            (setq vui-st--show-row (vui-with-async-context
                                     (vui-set-state :show t)))
            (if show
                (vui-fragment "A" (vui-component 'vui-st-silent-appender) "B")
              (vui-fragment))))

(describe "a hook that appends to a stream in the render of its last row"
  ;; The row ends the stream, so the stream ends where the row's render
  ;; writes: the render is in the stream there, and the item goes after
  ;; the row
  (it "puts the item after the row"
    (let ((vui-render-delay nil))
      (setq vui-st--at-point-stream (vui-make-stream))
      (vui-stream-append vui-st--at-point-stream (vui-text "first"))
      (vui-defcomponent vui-st-row-root ()
        :render (vui-vstack (vui-stream vui-st--at-point-stream)
                            (vui-text "bottom")))
      (let ((instance (vui-mount (vui-component 'vui-st-row-root)
                                 "*vui-st-row-at-point*")))
        (unwind-protect
            (with-current-buffer "*vui-st-row-at-point*"
              (vui-stream-append vui-st--at-point-stream
                                 (vui-component 'vui-st-growing-row))
              (funcall vui-st--show-row)
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "first\nAB\nadded\nbottom")
              (vui-stream-append vui-st--at-point-stream (vui-text "later"))
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "first\nAB\nadded\nlater\nbottom"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-row-at-point*"))))))

(describe "a hook that appends to a stream right before an error boundary"
  ;; The item is the stream's, not what the boundary's children wrote:
  ;; it stays when the boundary drops their text
  (it "keeps the item when the boundary catches"
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore))
      (setq vui-st--at-point-stream (vui-make-stream))
      (vui-stream-append vui-st--at-point-stream (vui-text "first"))
      (vui-defcomponent vui-st-boundary-root ()
        :render (vui-vstack
                 (vui-fragment
                  (vui-stream vui-st--at-point-stream)
                  (vui-error-boundary
                   :fallback (lambda (_) (vui-text "FB"))
                   :children (list (vui-component 'vui-st-silent-appender)
                                   "T"
                                   (vui-component 'vui-st-boom)))
                  "x")
                 "y"))
      (let ((instance (vui-mount (vui-component 'vui-st-boundary-root)
                                 "*vui-st-boundary-at-point*")))
        (unwind-protect
            (with-current-buffer "*vui-st-boundary-at-point*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "first\naddedFBx\ny")
              (vui-stream-append vui-st--at-point-stream (vui-text "later"))
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "first\nadded\nlaterFBx\ny"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-boundary-at-point*"))))))

(vui-defcomponent vui-st-text-appender ()
  "Render \"C\"; append \"added\" when mounted."
  :on-mount (vui-stream-append vui-st--at-point-stream (vui-text "added"))
  :render (vui-text "C"))

(describe "a hook that appends to a stream ending where an error boundary starts"
  ;; The child renders text before its hook appends: the item goes at
  ;; the end of the stream, which is the start of the boundary, before
  ;; that text.  It is the stream's, and stays when the boundary drops
  ;; the text of its children.
  (dolist (separator '("\n" ""))
    (it (format "keeps the item when the boundary catches (separator %S)"
                separator)
      (let ((vui-render-delay nil)
            (vui-lifecycle-error-handler 'ignore)
            (sep separator))
        (setq vui-st--at-point-stream (vui-make-stream separator))
        (vui-stream-append vui-st--at-point-stream (vui-text "first"))
        (vui-defcomponent vui-st-boundary-text-root ()
          :render (vui-vstack
                   (vui-fragment
                    (vui-stream vui-st--at-point-stream)
                    (vui-error-boundary
                     :fallback (lambda (_) (vui-text "FB"))
                     :children (list (vui-component 'vui-st-text-appender)
                                     (vui-component 'vui-st-boom)))
                    "x")
                   "y"))
        (let ((instance (vui-mount (vui-component 'vui-st-boundary-text-root)
                                   "*vui-st-boundary-text*")))
          (unwind-protect
              (with-current-buffer "*vui-st-boundary-text*"
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal (concat "first" sep "addedFBx\ny"))
                (expect (length (vui-stream-handle-items-rev
                                 vui-st--at-point-stream))
                        :to-equal 2)
                (vui-stream-append vui-st--at-point-stream (vui-text "later"))
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal (concat "first" sep "added" sep "laterFBx\ny"))
                (let ((root (vui-get-instance)))
                  (setf (vui-instance-render-record root) nil)
                  (vui-rerender root))
                (expect (buffer-substring-no-properties (point-min) (point-max))
                        :to-equal (concat "first" sep "added" sep "laterFBx\ny")))
            (vui-unmount instance)
            (kill-buffer "*vui-st-boundary-text*")))))))

(defvar vui-st--inner-stream nil
  "A stream inside the error boundary of `vui-st-boundary-inner-root'.")

(vui-defcomponent vui-st-two-appender ()
  "Render \"C\"; append to both streams when mounted."
  :on-mount (progn
              (vui-stream-append vui-st--at-point-stream (vui-text "added"))
              (vui-stream-append vui-st--inner-stream (vui-text "in2")))
  :render (vui-text "C"))

(vui-defcomponent vui-st-field-appender ()
  "Render \"C\"; append an item with a field when mounted."
  :on-mount (vui-stream-append vui-st--at-point-stream
                               (vui-fragment (vui-text "q:")
                                             (vui-field :value "ab" :size 3)))
  :render (vui-text "C"))

(describe "a boundary that catches, after a stream that took a hook's item"
  (it "drops a stream it holds"
    ;; That stream started where the boundary starts: it goes with the
    ;; children's text, and its edits write nothing
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore))
      (setq vui-st--at-point-stream (vui-make-stream "")
            vui-st--inner-stream (vui-make-stream ""))
      (vui-stream-append vui-st--at-point-stream (vui-text "out"))
      (vui-stream-append vui-st--inner-stream (vui-text "in"))
      (vui-defcomponent vui-st-boundary-inner-root ()
        :render (vui-vstack
                 (vui-fragment
                  (vui-stream vui-st--at-point-stream)
                  (vui-error-boundary
                   :fallback (lambda (_) (vui-text "FB"))
                   :children (list (vui-stream vui-st--inner-stream)
                                   (vui-component 'vui-st-two-appender)
                                   (vui-component 'vui-st-boom)))
                  "x")
                 "y"))
      (let ((instance (vui-mount (vui-component 'vui-st-boundary-inner-root)
                                 "*vui-st-boundary-inner*")))
        (unwind-protect
            (with-current-buffer "*vui-st-boundary-inner*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "outaddedFBx\ny")
              (vui-stream-append vui-st--inner-stream (vui-text "L"))
              (vui-stream-append vui-st--at-point-stream (vui-text "M"))
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "outaddedMFBx\ny"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-boundary-inner*")))))

  (it "keeps the field of the item out of the fallback"
    (let ((vui-render-delay nil)
          (vui-lifecycle-error-handler 'ignore))
      (setq vui-st--at-point-stream (vui-make-stream ""))
      (vui-stream-append vui-st--at-point-stream (vui-text "first"))
      (vui-defcomponent vui-st-boundary-field-root ()
        :render (vui-vstack
                 (vui-fragment
                  (vui-stream vui-st--at-point-stream)
                  (vui-error-boundary
                   :fallback (lambda (_) (vui-text "FB"))
                   :children (list (vui-component 'vui-st-field-appender)
                                   (vui-component 'vui-st-boom)))
                  "x")
                 "y"))
      (let ((instance (vui-mount (vui-component 'vui-st-boundary-field-root)
                                 "*vui-st-boundary-field*")))
        (unwind-protect
            (with-current-buffer "*vui-st-boundary-field*"
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "firstq:ab FBx\ny")
              (goto-char (point-min))
              (search-forward "ab")
              (insert "Z")
              (expect (widget-value (widget-field-at (1- (point))))
                      :to-equal "abZ")
              (expect (buffer-substring-no-properties (point-min) (point-max))
                      :to-equal "firstq:abZFBx\ny"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-boundary-field*"))))))

(defun vui-st--boom-on (vnode)
  "Signal when VNODE is the text \"BOOM\"."
  (when (and (vui-vnode-text-p vnode)
             (equal (vui-vnode-text-content vnode) "BOOM"))
    (error "Boom")))

(vui-defcomponent vui-st-failing-write-root (stream)
  :render (vui-vstack (vui-text "top") (vui-stream stream) (vui-text "bottom")))

(describe "a write of a stream in place that signals"
  ;; The write does not happen: what it wrote goes, and so does the edit
  ;; of the stream's items, and the error goes on
  (dolist (write '(append open update-last update append-to before after))
    (it (format "leaves the stream as it was (%s)" write)
      (let ((vui-render-delay nil)
            (stream (vui-make-stream))
            (node nil))
        (vui-stream-append stream (vui-text "first"))
        (setq node (vui-stream-open stream (vui-text "node")))
        (let ((instance (vui-mount (vui-component 'vui-st-failing-write-root
                                                  :stream stream)
                                   "*vui-st-failing*"))
              (vnode (vui-fragment (vui-text "x")
                                   (vui-field :value "f" :size 5)
                                   (vui-text "BOOM"))))
          (unwind-protect
              (with-current-buffer "*vui-st-failing*"
                (advice-add 'vui--render-vnode :before #'vui-st--boom-on)
                (unwind-protect
                    (expect (pcase write
                              ('append (vui-stream-append stream vnode))
                              ('open (vui-stream-open stream vnode))
                              ('update-last (vui-stream-update-last stream vnode))
                              ('update (vui-stream-update node vnode))
                              ('append-to (vui-stream-append-to node vnode))
                              ('before (vui-stream-before node vnode))
                              ('after (vui-stream-after node vnode)))
                            :to-throw 'error)
                  (advice-remove 'vui--render-vnode #'vui-st--boom-on))
                (expect (buffer-string) :to-equal "top\nfirst\nnode\nbottom")
                (expect widget-field-list :to-be nil)
                (expect widget-field-new :to-be nil)
                (expect (length (vui-stream-handle-items-rev stream))
                        :to-equal 2)
                (vui-stream-append stream (vui-text "later"))
                (expect (buffer-string)
                        :to-equal "top\nfirst\nnode\nlater\nbottom")
                (let ((root (vui-get-instance)))
                  (setf (vui-instance-render-record root) nil)
                  (vui-rerender root))
                (expect (buffer-string)
                        :to-equal "top\nfirst\nnode\nlater\nbottom"))
            (vui-unmount instance)
            (kill-buffer "*vui-st-failing*")))))))

(defvar vui-st--failing-hook-runs 0
  "How many times `vui-st-failing-hook' updated.")

(defvar vui-st--failing-hook-stream nil
  "The stream `vui-st-failing-hook' writes.")

(vui-defcomponent vui-st-failing-hook ()
  "Writes the last item of a stream on each update, with a write that fails."
  :on-update (progn
               (cl-incf vui-st--failing-hook-runs)
               (ignore-errors
                 (vui-stream-update-last vui-st--failing-hook-stream
                                         (vui-text "BOOM"))))
  :render (vui-text "hk"))

(vui-defcomponent vui-st-failing-hook-root (stream)
  :render (vui-vstack (vui-stream stream) (vui-component 'vui-st-failing-hook)))

(describe "a write of a stream in place that signals, more cases"
  (it "puts a static last item back (update-last, rewrite and extend)"
    (dolist (new '("BOOM" "lastBOOM"))
      (let ((vui-render-delay nil)
            (stream (vui-make-stream)))
        (vui-stream-append stream (vui-text "first"))
        (vui-stream-append stream (vui-text "last"))
        (let ((instance (vui-mount (vui-component 'vui-st-failing-write-root
                                                  :stream stream)
                                   "*vui-st-failing*")))
          (unwind-protect
              (with-current-buffer "*vui-st-failing*"
                (advice-add 'vui--render-vnode :before #'vui-st--boom-on)
                (unwind-protect
                    (expect (vui-stream-update-last stream (vui-text new))
                            :to-throw 'error)
                  (advice-remove 'vui--render-vnode #'vui-st--boom-on))
                (expect (buffer-string) :to-equal "top\nfirst\nlast\nbottom")
                (vui-stream-update-last stream (vui-text "UPD"))
                (expect (buffer-string) :to-equal "top\nfirst\nUPD\nbottom"))
            (vui-unmount instance)
            (kill-buffer "*vui-st-failing*"))))))

  (it "leaves point where it was"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-stream-append stream (vui-text "a"))
      (let ((instance (vui-mount (vui-component 'vui-st-failing-write-root
                                                :stream stream)
                                 "*vui-st-failing*")))
        (unwind-protect
            (with-current-buffer "*vui-st-failing*"
              (goto-char (point-max))
              (let ((before (point)))
                (advice-add 'vui--render-vnode :before #'vui-st--boom-on)
                (unwind-protect
                    (ignore-errors (vui-stream-append stream (vui-text "BOOM")))
                  (advice-remove 'vui--render-vnode #'vui-st--boom-on))
                (expect (point) :to-equal before)))
          (vui-unmount instance)
          (kill-buffer "*vui-st-failing*")))))

  (it "keeps the field of an item it puts back"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream))
          (node nil))
      (setq node (vui-stream-open stream (vui-fragment
                                          (vui-text "q:")
                                          (vui-field :value "ab" :size 4))))
      (let ((instance (vui-mount (vui-component 'vui-st-failing-write-root
                                                :stream stream)
                                 "*vui-st-failing*")))
        (unwind-protect
            (with-current-buffer "*vui-st-failing*"
              (advice-add 'vui--render-vnode :before #'vui-st--boom-on)
              (unwind-protect
                  (ignore-errors (vui-stream-update node (vui-text "BOOM")))
                (advice-remove 'vui--render-vnode #'vui-st--boom-on))
              (expect (buffer-string) :to-equal "top\nq:ab  \nbottom")
              (goto-char (point-min))
              (search-forward "ab")
              (insert "Z")
              (expect (widget-value (widget-field-at (1- (point))))
                      :to-equal "abZ"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-failing*")))))

  (it "runs a hook that writes on each render once"
    ;; The write that fails does not ask for another render
    (let ((vui-render-delay nil)
          (vui-st--failing-hook-runs 0))
      (setq vui-st--failing-hook-stream (vui-make-stream))
      (vui-stream-append vui-st--failing-hook-stream (vui-text "a"))
      (vui-stream-append vui-st--failing-hook-stream (vui-text "b"))
      (let ((instance (vui-mount (vui-component 'vui-st-failing-hook-root
                                                :stream vui-st--failing-hook-stream)
                                 "*vui-st-failing*")))
        (unwind-protect
            (with-current-buffer "*vui-st-failing*"
              (advice-add 'vui--render-vnode :before #'vui-st--boom-on)
              (unwind-protect
                  (vui-rerender (vui-get-instance))
                (advice-remove 'vui--render-vnode #'vui-st--boom-on))
              (expect vui-st--failing-hook-runs :to-equal 1)
              (expect (buffer-string) :to-equal "a\nb\nhk"))
          (vui-unmount instance)
          (kill-buffer "*vui-st-failing*"))))))

(provide 'vui-stream-test)
;;; vui-stream-test.el ends here

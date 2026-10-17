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

(provide 'vui-stream-test)
;;; vui-stream-test.el ends here

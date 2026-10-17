;;; vui-stream-rows-test.el --- Stateful component rows in a vui-stream -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A `vui-stream' item may be a `vui-component', mounted as a STATEFUL ROW:
;; it has its own region and re-renders only that region when its state
;; changes (a collapsible tool card, say), independent of the number of
;; items above it.  These tests prove:
;;   PARITY  - a stream of rows renders identically to the same components
;;             rendered declaratively as children, including after state
;;             changes (expand / collapse);
;;   COEXIST - expanding the last row keeps the box below correct (the
;;             stream's region-end follows the row's growth), and content
;;             appended after rows lands correctly;
;;   SCOPED  - expanding a row leaves the box region untouched (the row
;;             re-renders only itself).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'vui)
(require 'cl-lib)

;; A collapsible row: the button toggles an extra detail line.
(vui-defcomponent vui-sr-row (id)
  :state ((open nil))
  :render
  (vui-vstack
   (vui-button (format "[%s] row %s" (if open "v" ">") id)
     :on-click (lambda () (vui-set-state :open (not open))))
   (when open
     (vui-text (format "   detail line for %s" id)))))

;; Stream UI: rows live in the stream, a box sits below.
(vui-defcomponent vui-sr-app (stream)
  :render (vui-vstack (vui-stream stream)
                      (vui-text "==== box (idle) ====")))

;; Declarative equivalent: the same components as plain vstack children.
(vui-defcomponent vui-sr-decl ()
  :render (vui-vstack
           (vui-text "system: started")
           (vui-component 'vui-sr-row :key "A" :id "A")
           (vui-component 'vui-sr-row :key "B" :id "B")
           (vui-text "system: more")
           (vui-text "==== box (idle) ====")))

(defun vui-sr--kill (buf)
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t)) (kill-buffer buf)))))

(defun vui-sr--click (buf label)
  "Invoke the button whose text contains LABEL in BUF."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (search-forward label nil t)
      (let ((w (button-at (match-beginning 0))))
        (when w (button-activate w))))))

(defun vui-sr--build-stream (&rest clicks)
  "Build the stream UI (content + 2 rows + content), apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-s*")
         (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (vui-stream-append s (vui-text "system: started"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
          (vui-stream-append s (vui-component 'vui-sr-row :id "B"))
          (vui-stream-append s (vui-text "system: more"))
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(defun vui-sr--build-decl (&rest clicks)
  "Build the declarative UI, apply CLICKS, return buffer."
  (let* ((vui-render-delay nil) (buf "*vui-sr-d*")
         (inst (vui-mount (vui-component 'vui-sr-decl) buf)))
    (ignore inst)
    (unwind-protect
        (progn
          (dolist (c clicks) (vui-sr--click buf c))
          (with-current-buffer buf (buffer-string)))
      (vui-sr--kill buf))))

(describe "vui-stream rows: parity with declarative children"
  (it "renders identically with all rows collapsed"
    (expect (vui-sr--build-stream) :to-equal (vui-sr--build-decl)))

  (it "renders identically after expanding both rows"
    (expect (vui-sr--build-stream "row A" "row B")
            :to-equal (vui-sr--build-decl "row A" "row B")))

  (it "renders identically after expanding only the last row"
    (expect (vui-sr--build-stream "row B")
            :to-equal (vui-sr--build-decl "row B")))

  (it "renders identically after expand-then-collapse (round trip)"
    (expect (vui-sr--build-stream "row A" "row A")
            :to-equal (vui-sr--build-decl))))

(describe "vui-stream rows: coexistence with the box below"
  (it "keeps the box correct and last after expanding the last row"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-co*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (vui-sr--click buf "row A")     ; expand the last (only) row
            (with-current-buffer buf
              ;; region-end matches the live end of stream content (just
              ;; before the box separator), and the box is still last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)
              (expect (buffer-string) :to-match "detail line for A")))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: re-render is scoped to the row"
  (it "leaves the box region untouched when a row expands"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-sc*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "A"))
            (with-current-buffer buf
              ;; a marker inside the box; expanding the row must not erase
              ;; or rewrite the box (only the row's region changes)
              (goto-char (point-max))
              (search-backward "box")
              (let* ((m (copy-marker (point)))
                     (ch (char-after m)))
                (vui-sr--click buf "row A")
                (expect (char-after m) :to-equal ch)
                (expect (buffer-substring m (+ m 3)) :to-equal "box"))))
        (vui-sr--kill buf)))))

(describe "vui-stream rows: update-last refreshes a row's props in place"
  (it "updates the row's props while keeping its state"
    (let* ((vui-render-delay nil) (s (vui-make-stream)) (buf "*vui-sr-ul*")
           (inst (vui-mount (vui-component 'vui-sr-app :stream s) buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "system: started"))
            (vui-stream-append s (vui-component 'vui-sr-row :id "Z"))
            (vui-sr--click buf "row Z")          ; expand: state open = t
            (with-current-buffer buf
              (expect (buffer-string) :to-match "detail line for Z"))
            ;; new data arrives for the in-progress row
            (vui-stream-update-last s (vui-component 'vui-sr-row :id "Z2"))
            (with-current-buffer buf
              ;; props refreshed (Z -> Z2)...
              (expect (buffer-string) :to-match "row Z2")
              ;; ...and the row's expanded state survived the update
              (expect (buffer-string) :to-match "detail line for Z2")
              ;; box still correct and last
              (expect (string-suffix-p "==== box (idle) ====" (buffer-string))
                      :to-be t)))
        (vui-sr--kill buf)))))

;; --- A rebuild of the stream re-seats its rows ---
;;
;; A row renders on its own, in a region of its own inside the stream.
;; A render that rewrites the stream erases that region: the root showed
;; another tree in between, its render was nil, the stream is not the
;; first child, or the component that owns the stream renders again.
;; The rebuild must put every row back at its new place, so the row
;; keeps its state and still re-renders on its own afterwards.  While
;; the stream is not in the buffer, a row has no place to write to.

(defconst vui-sr-gap--buffer "*vui-sr-gap*")

(defvar vui-sr-gap--stream nil
  "The stream handle of the last render that placed one.")

(defvar vui-sr-gap--log nil
  "Lifecycle events of the rows, newest first.")

(defvar vui-sr-gap--bumps nil
  "Alist of a row's label to a function that adds 1 to its count.
The function is what a timer or a process callback of the row runs.")

(defvar vui-sr-gap--root-renders 0
  "Number of renders of `vui-sr-gap-root'.")

;; A row with state.  Its button adds 1 to the count, and from a count
;; of 2 it shows a second line.
(vui-defcomponent vui-sr-gap-row (label)
  :state ((n 0))
  :on-mount (progn
              (push (cons 'mount label) vui-sr-gap--log)
              (push (cons label (vui-with-async-context
                                  (vui-set-state :n #'1+)))
                    vui-sr-gap--bumps)
              nil)
  :on-unmount (push (cons 'unmount label) vui-sr-gap--log)
  :render (vui-vstack
           (vui-button (format "%s:%d" label n)
             :on-click (lambda () (vui-set-state :n (1+ n))))
           (when (> n 1)
             (vui-text (format "  %s more" label)))))

;; A row that shows what an effect copied from its label.
(vui-defcomponent vui-sr-gap-echo (label)
  :state ((seen nil))
  :render (progn
            (vui-use-effect (label)
              (vui-set-state :seen label))
            (vui-text (format "echo %s" seen))))

(vui-defcomponent vui-sr-gap-root (mode)
  :render
  (let ((h (vui-use-stream)))
    (setq vui-sr-gap--stream h)
    (cl-incf vui-sr-gap--root-renders)
    (pcase mode
      ('tail (vui-vstack (vui-stream h) (vui-text "footer")))
      ('head (vui-vstack (vui-text "header") (vui-stream h) (vui-text "footer")))
      ('other (vui-text "other"))
      ('none nil))))

;; One line in a box, which pads it to the right by its measured width
(vui-defcomponent vui-sr-gap-boxed ()
  :render (let ((h (vui-use-stream " ")))
            (setq vui-sr-gap--stream h)
            (vui-box (vui-stream h) :width 20 :align :right)))

(defun vui-sr-gap--mount (mode)
  "Mount `vui-sr-gap-root' in MODE and return it.
MODE `boxed' mounts `vui-sr-gap-boxed' instead."
  (setq vui-sr-gap--log nil
        vui-sr-gap--bumps nil
        vui-sr-gap--root-renders 0)
  (vui-mount (if (eq mode 'boxed)
                 (vui-component 'vui-sr-gap-boxed)
               (vui-component 'vui-sr-gap-root :mode mode))
             vui-sr-gap--buffer))

(defun vui-sr-gap--text ()
  (with-current-buffer vui-sr-gap--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vui-sr-gap--click (label)
  "Press the button of the row with LABEL."
  (vui-sr--click vui-sr-gap--buffer (format "[%s:" label)))

(defun vui-sr-gap--bump (label)
  "Add 1 to the count of the row with LABEL, as a timer of the row would."
  (funcall (cdr (assoc label vui-sr-gap--bumps))))

(defun vui-sr-gap--resize ()
  "Re-render the buffer's instances as a change of the window width does."
  (with-current-buffer vui-sr-gap--buffer
    (vui--on-window-size-change nil)))

(defun vui-sr-gap--path (label)
  "Return the render path of the button of the row with LABEL."
  (with-current-buffer vui-sr-gap--buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward (format "[%s:" label))
      (vui--elt-get (button-at (match-beginning 0)) :vui-path))))

(describe "vui-stream rows: a rebuild of the stream re-seats them"
  (dolist (flag '(nil t))
    (it (format "keeps the rows when the root shows another tree in between (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'tail)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r2"))
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              (vui-update root '(:mode other))
              (expect (vui-sr-gap--text) :to-equal "other")
              (vui-update root '(:mode tail))
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              ;; A resize re-renders the root and every row
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:0]\nfooter")
              ;; Each row still re-renders on its own, in its own place
              (vui-sr-gap--click "r2")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\n[r2:1]\nfooter")
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text)
                      :to-equal "a\n[r1:2]\n  r1 more\n[r2:1]\nfooter")
              ;; What is appended after the rows lands after them
              (vui-stream-append s (vui-text "b"))
              (expect (vui-sr-gap--text)
                      :to-equal "a\n[r1:2]\n  r1 more\n[r2:1]\nb\nfooter")
              ;; The rows were not mounted again, nor unmounted
              (expect (reverse vui-sr-gap--log)
                      :to-equal '((mount . "r1") (mount . "r2"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (dolist (gap '(other none))
    (dolist (flag '(nil t))
      (it (format "writes nothing for a row while the root renders %s (flag %s)"
                  (if (eq gap 'none) "nil" "another tree")
                  (if flag "on" "off"))
        (let ((vui-incremental-render flag)
              (vui-render-delay nil)
              (root (vui-sr-gap--mount 'tail))
              (shown (if (eq gap 'none) "" "other")))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root (list :mode gap))
                (expect (vui-sr-gap--text) :to-equal shown)
                ;; A state change of the row, a resize, new props: the row
                ;; is not in the buffer, so none of them writes to it
                (vui-sr-gap--bump "r1")
                (expect (vui-sr-gap--text) :to-equal shown)
                (vui-sr-gap--resize)
                (expect (vui-sr-gap--text) :to-equal shown)
                (vui-stream-update-last
                 s (vui-component 'vui-sr-gap-row :label "r1x"))
                (expect (vui-sr-gap--text) :to-equal shown)
                ;; The row shows all of it when the stream comes back
                (vui-update root '(:mode tail))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1x:1]\nfooter")
                (vui-sr-gap--click "r1x")
                (expect (vui-sr-gap--text)
                        :to-equal "a\n[r1x:2]\n  r1x more\nfooter"))
            (vui-sr--kill vui-sr-gap--buffer))))))

  (dolist (flag '(nil t))
    (it (format "keeps the rows when the root renders nil in between (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'tail)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-sr-gap--click "r1")
              (vui-update root '(:mode none))
              (vui-update root '(:mode tail))
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nfooter")
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nfooter")
              (vui-sr-gap--click "r1")
              (expect (vui-sr-gap--text) :to-equal "a\n[r1:2]\n  r1 more\nfooter")
              (expect (reverse vui-sr-gap--log) :to-equal '((mount . "r1"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (dolist (flag '(nil t))
    (it (format "keeps the rows when the stream is not the first child (flag %s)"
                (if flag "on" "off"))
      ;; Every render of the root rebuilds the stream here
      (let ((vui-incremental-render flag)
            (vui-render-delay nil)
            (root (vui-sr-gap--mount 'head)))
        (unwind-protect
            (let ((s vui-sr-gap--stream))
              (vui-stream-append s (vui-text "a"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
              (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r2"))
              (vui-rerender root)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:0]\n[r2:0]\nfooter")
              ;; A row re-renders on its own, not through the root
              (let ((renders vui-sr-gap--root-renders))
                (vui-sr-gap--click "r1")
                (expect vui-sr-gap--root-renders :to-equal renders))
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              ;; The next rebuild keeps the row's state
              (vui-rerender root)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              (vui-sr-gap--resize)
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:0]\nfooter")
              ;; The last row grows after a rebuild: the end of the stream
              ;; follows it, so an append lands after the new line
              (vui-sr-gap--click "r2")
              (vui-sr-gap--click "r2")
              (vui-stream-append s (vui-text "b"))
              (expect (vui-sr-gap--text)
                      :to-equal "header\na\n[r1:1]\n[r2:2]\n  r2 more\nb\nfooter")
              (expect (reverse vui-sr-gap--log)
                      :to-equal '((mount . "r1") (mount . "r2"))))
          (vui-sr--kill vui-sr-gap--buffer)))))

  (it "keeps the stream in the buffer when its only item, a row, re-renders"
    ;; The row and the stream have the same text here.  The re-render
    ;; deletes the text of the row, not of the stream.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (ignore root)
      (unwind-protect
          (let* ((s vui-sr-gap--stream)
                 (n (vui-stream-open s (vui-text "x"))))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (vui-stream-remove n)
            (expect (vui-sr-gap--text) :to-equal "[r1:0]\nfooter")
            (vui-sr-gap--click "r1")
            (vui-stream-append s (vui-text "b"))
            (expect (vui-sr-gap--text) :to-equal "[r1:1]\nb\nfooter"))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "gives a row the paths of its new place in the tree"
    ;; The stream is child 0 of the root's vstack, then child 1.  The row
    ;; is item 1 of the stream, and its button child 0 of the row.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (expect (vui-sr-gap--path "r1") :to-equal '(0 1 0))
            (vui-update root '(:mode head))
            (expect (vui-sr-gap--path "r1") :to-equal '(1 1 0))
            ;; and keeps them when it re-renders on its own
            (vui-sr-gap--click "r1")
            (expect (vui-sr-gap--path "r1") :to-equal '(1 1 0)))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "re-renders a row whose effect changes its state after a rebuild"
    ;; The props of the row change while the stream is out of the buffer,
    ;; so its effect runs after the render that brings the stream back.
    ;; The state change it makes re-renders the row, not the root (which
    ;; would patch around the stream and leave the row as it was).
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'tail)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-echo :label "x"))
            (expect (vui-sr-gap--text) :to-equal "a\necho x\nfooter")
            (vui-update root '(:mode other))
            (vui-stream-update-last s (vui-component 'vui-sr-gap-echo :label "y"))
            (vui-update root '(:mode tail))
            (expect (vui-sr-gap--text) :to-equal "a\necho y\nfooter"))
        (vui-sr--kill vui-sr-gap--buffer))))

  (it "measures the rows with the rest of the stream"
    ;; The box pads its content by the width it measured.  A row that is
    ;; rendered but not measured would leave the line too wide.
    (let ((vui-render-delay nil)
          (root (vui-sr-gap--mount 'boxed)))
      (unwind-protect
          (let ((s vui-sr-gap--stream))
            (vui-stream-append s (vui-text "a"))
            (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
            (vui-rerender root)
            (expect (vui-sr-gap--text)
                    :to-equal (concat (make-string 12 ?\s) "a [r1:0]")))
        (vui-sr--kill vui-sr-gap--buffer)))))

;; --- The component that owns the stream ---

(vui-defcomponent vui-sr-gap-chat ()
  :render (let ((h (vui-use-stream)))
            (setq vui-sr-gap--stream h)
            (vui-stream h)))

;; Every render of the shell renders the chat again, which rebuilds the
;; stream
(vui-defcomponent vui-sr-gap-shell (chat tick)
  :render (vui-vstack
           (when chat (vui-component 'vui-sr-gap-chat :key "chat"))
           (vui-text (format "box %s" tick))))

(describe "vui-stream rows: the component that owns the stream"
  (dolist (flag '(nil t))
    (it (format "keeps the rows when it renders again (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil))
        (setq vui-sr-gap--log nil vui-sr-gap--bumps nil)
        (let ((root (vui-mount (vui-component 'vui-sr-gap-shell :chat t :tick 0)
                               vui-sr-gap--buffer)))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root '(:chat t :tick 1))
                (vui-update root '(:chat t :tick 2))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:0]\nbox 2")
                (vui-sr-gap--click "r1")
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nbox 2")
                (vui-update root '(:chat t :tick 3))
                (expect (vui-sr-gap--text) :to-equal "a\n[r1:1]\nbox 3"))
            (vui-sr--kill vui-sr-gap--buffer))))))

  (dolist (flag '(nil t))
    (it (format "unmounts the rows when it unmounts (flag %s)"
                (if flag "on" "off"))
      (let ((vui-incremental-render flag)
            (vui-render-delay nil))
        (setq vui-sr-gap--log nil vui-sr-gap--bumps nil)
        (let ((root (vui-mount (vui-component 'vui-sr-gap-shell :chat t :tick 0)
                               vui-sr-gap--buffer)))
          (unwind-protect
              (let ((s vui-sr-gap--stream))
                (vui-stream-append s (vui-text "a"))
                (vui-stream-append s (vui-component 'vui-sr-gap-row :label "r1"))
                (vui-update root '(:chat t :tick 1))
                (vui-update root '(:chat nil :tick 2))
                (expect (vui-sr-gap--text) :to-equal "box 2")
                (expect (reverse vui-sr-gap--log)
                        :to-equal '((mount . "r1") (unmount . "r1")))
                (with-current-buffer vui-sr-gap--buffer
                  (expect vui--inline-instances :to-equal nil))
                ;; Nothing of the row comes back
                (vui-sr-gap--resize)
                (vui-sr-gap--bump "r1")
                (expect (vui-sr-gap--text) :to-equal "box 2"))
            (vui-sr--kill vui-sr-gap--buffer)))))))

;; A row that shows its text, and renders nothing at all without one (a
;; tool card that waits for its result).
(vui-defcomponent vui-sr-quiet (text)
  :render (vui-text (or text "")))

(vui-defcomponent vui-sr-status (stream)
  :render (vui-vstack (vui-stream stream) (vui-text "status")))

(defmacro vui-sr--with-status-stream (spec &rest body)
  "Run BODY in the buffer of a live stream with a status line below it.
SPEC is (STREAM ROOT [SEPARATOR]): two symbols, bound to the stream
handle and to the root instance, and the separator of the stream's items
\(default a newline)."
  (declare (indent 1))
  (pcase-let ((`(,stream ,root ,separator) spec))
    `(let* ((vui-render-delay nil)
            (,stream (vui-make-stream ,separator))
            (,root (vui-mount (vui-component 'vui-sr-status :stream ,stream)
                              "*vui-sr-q*")))
       (ignore ,root)
       (unwind-protect
           (with-current-buffer "*vui-sr-q*" ,@body)
         (vui-sr--kill "*vui-sr-q*")))))

(defun vui-sr--append-quiet (stream text)
  "Append a `vui-sr-quiet' row showing TEXT to STREAM and return the row."
  (vui-stream-append stream (vui-component 'vui-sr-quiet :text text))
  (car (vui-stream-handle-items-rev stream)))

(defun vui-sr--row-bounds (row)
  "Return (START . END), where the region markers of ROW are."
  (cons (marker-position (vui-instance-region-start row))
        (marker-position (vui-instance-region-end row))))

(describe "vui-stream rows: a row that renders nothing"
  (it "keeps its place when an item is appended after it"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "")))
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string) :to-equal "hello\n\nworld\nstatus")
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps its place when a row is appended after it"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let* ((first (vui-sr--append-quiet stream ""))
             (second (vui-sr--append-quiet stream "second")))
        (expect (buffer-string) :to-equal "hello\n\nsecond\nstatus")
        (vui-update first '(:text "first"))
        (expect (buffer-string) :to-equal "hello\nfirst\nsecond\nstatus")
        (vui-update second '(:text "second, longer"))
        (expect (buffer-string)
                :to-equal "hello\nfirst\nsecond, longer\nstatus"))))

  (it "keeps its place when the content below the stream is re-rendered"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "")))
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\n\nstatus")
        (expect (vui-sr--row-bounds row) :to-equal '(7 . 7))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        ;; The stream still ends where its last row ends
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps its place when it loses its text again"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "running")))
        (vui-update row '(:text ""))
        (vui-rerender root)
        (vui-stream-append stream (vui-text "world"))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "is updated through its node"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((node (vui-stream-open stream
                                   (vui-component 'vui-sr-quiet :text ""))))
        (vui-stream-append stream (vui-text "world"))
        (vui-stream-update node (vui-component 'vui-sr-quiet
                                               :text "TOOL RESULT"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "is the last item again once the node after it is removed"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream ""))
            (node (vui-stream-open stream (vui-text "thinking"))))
        (vui-stream-remove node)
        (expect (buffer-string) :to-equal "hello\n\nstatus")
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus"))))

  (it "keeps a row that renders nothing when it is the only item left"
    ;; The stream has no text, so the append lays the tree out again, and
    ;; that writes the row again at its place
    (vui-sr--with-status-stream (stream root)
      (let* ((note (vui-stream-open stream (vui-text "note")))
             (row (vui-sr--append-quiet stream "")))
        (vui-stream-remove note)
        (vui-rerender root)
        (vui-stream-append stream (vui-text "world"))
        (vui-update row '(:text "TOOL RESULT"))
        (expect (buffer-string) :to-equal "TOOL RESULT\nworld\nstatus")
        (vui-rerender root)
        (expect (buffer-string) :to-equal "TOOL RESULT\nworld\nstatus"))))

  (it "keeps its place among rows that no separator sets apart"
    ;; Whichever of them gets its text first
    (dolist (order '((first second) (second first)))
      (vui-sr--with-status-stream (stream root "")
        (vui-stream-append stream (vui-text "hello "))
        (let ((first (vui-sr--append-quiet stream ""))
              (second (vui-sr--append-quiet stream "")))
          (dolist (name order)
            (pcase name
              ('first (vui-update first '(:text "first ")))
              ('second (vui-update second '(:text "second ")))))
          (expect (buffer-string) :to-equal "hello first second \nstatus")
          (vui-stream-append stream (vui-text "world"))
          (expect (buffer-string)
                  :to-equal "hello first second world\nstatus"))))))

(describe "vui-stream rows: the node after a row is removed"
  (it "leaves the stream ending where the row ends"
    (vui-sr--with-status-stream (stream root)
      (vui-stream-append stream (vui-text "hello"))
      (let ((row (vui-sr--append-quiet stream "running"))
            (node (vui-stream-open stream (vui-text "thinking"))))
        (vui-stream-remove node)
        (vui-update row '(:text "TOOL RESULT"))
        (vui-rerender root)
        (expect (buffer-string) :to-equal "hello\nTOOL RESULT\nstatus")
        (vui-stream-append stream (vui-text "world"))
        (expect (buffer-string)
                :to-equal "hello\nTOOL RESULT\nworld\nstatus")))))

(defun vui-sr--insert-next-to (lines id new after)
  "Return LINES with NEW before the entry of ID, or after it when AFTER."
  (let ((result nil))
    (dolist (line lines)
      (cond
       ((not (eq (car line) id)) (push line result))
       (after (push line result) (push new result))
       (t (push new result) (push line result))))
    (nreverse result)))

(describe "vui-stream rows: random edits around rows that render nothing"
  ;; Seeded runs of random operations on a stream of plain items, rows
  ;; and content nodes, against a model: the list of the items' texts.
  ;; Half of the rows render nothing, and a row changes its text at any
  ;; time, so empty rows get an item appended after them, a node
  ;; inserted or removed next to them, and the content below the stream
  ;; re-rendered.  The buffer has to hold the texts, in order, after
  ;; every operation.
  (it "keeps every item in its place"
    (let ((failure nil))
      (cl-block done
        (dotimes (seed 40)
          (random (format "vui-stream-rows-%d" seed))
          (vui-sr--with-status-stream (stream root)
            (vui-stream-append stream (vui-text "hello"))
            (let ((lines (list (cons -1 "hello"))) ; (ID . TEXT), in order
                  (rows nil)                       ; (ID . ROW)
                  (nodes nil)                      ; (ID . NODE), content
                  (next 0)
                  (trace nil))
              (dotimes (step 40)
                (let* ((ops (append '(append open row row rerender)
                                    (when rows '(update-row update-row))
                                    (when nodes '(update before after remove))))
                       (op (nth (random (length ops)) ops))
                       (text (format "x%d" step))
                       (row-text (if (zerop (random 2)) "" text))
                       (row (and rows (nth (random (length rows)) rows)))
                       (node (and nodes (nth (random (length nodes)) nodes)))
                       (new (cons next text)))
                  (push (list op (car (if (eq op 'update-row) row node))
                              row-text)
                        trace)
                  (pcase op
                    ('append
                     (vui-stream-append stream (vui-text text))
                     (setq lines (append lines (list new))))
                    ('open
                     (push (cons next (vui-stream-open stream (vui-text text)))
                           nodes)
                     (setq lines (append lines (list new))))
                    ('row
                     (push (cons next (vui-sr--append-quiet stream row-text))
                           rows)
                     (setq lines (append lines (list (cons next row-text)))))
                    ('rerender
                     (vui-rerender root))
                    ('update-row
                     (vui-update (cdr row) (list :text row-text))
                     (setcdr (assq (car row) lines) row-text))
                    ('update
                     (vui-stream-update (cdr node) (vui-text row-text))
                     (setcdr (assq (car node) lines) row-text))
                    ((or 'before 'after)
                     (push (cons next
                                 (funcall (if (eq op 'before)
                                              #'vui-stream-before
                                            #'vui-stream-after)
                                          (cdr node) (vui-text text)))
                           nodes)
                     (setq lines (vui-sr--insert-next-to
                                  lines (car node) new (eq op 'after))))
                    ('remove
                     (vui-stream-remove (cdr node))
                     (setq lines (assq-delete-all (car node) lines)
                           nodes (assq-delete-all (car node) nodes))))
                  (setq next (1+ next))
                  (let ((expected (concat (mapconcat #'cdr lines "\n")
                                          "\nstatus")))
                    (unless (equal (buffer-string) expected)
                      (setq failure (list :seed seed
                                          :trace (reverse trace)
                                          :expected expected
                                          :got (buffer-string)))
                      (cl-return-from done)))))))))
      (expect failure :to-be nil))))

;; A row that runs HOOK when it mounts: from inside its first render.
(vui-defcomponent vui-sr-hook-row (label hook)
  :on-mount (when hook (funcall hook))
  :render (vui-text (if label (format "[%s]" label) "")))

;; The same, with HOOK run by the renders after the first one.
(vui-defcomponent vui-sr-hook-update-row (label hook)
  :on-update (when hook (funcall hook))
  :render (vui-text (format "[%s]" label)))

(vui-defcomponent vui-sr-hook-panel (stream)
  :render (vui-vstack (vui-text "panel") (vui-stream stream)))

;; LOG is the first child with MODE `first', and comes after a line "x"
;; otherwise.
(vui-defcomponent vui-sr-hook-root (log panel mode)
  :render (apply #'vui-vstack
                 (append (unless (eq mode 'first) (list (vui-text "x")))
                         (list (vui-stream log)
                               (vui-component 'vui-sr-hook-panel :stream panel)
                               (vui-text "footer")))))

(defmacro vui-sr-hook--with (mode separator &rest body)
  "Run BODY in the buffer of a root with two live streams, LOG and PANEL.
The root renders them with MODE (see `vui-sr-hook-root').  LOG has the
item \"l0\" and PANEL has none, and SEPARATOR sets apart the items of
PANEL (default a newline).  BODY refers to the streams as `log' and
`panel', and runs after a second render of the root."
  (declare (indent 2))
  `(let* ((vui-render-delay nil)
          (log (vui-make-stream))
          (panel (vui-make-stream ,separator)))
     (vui-stream-append log (vui-text "l0"))
     (vui-mount (vui-component 'vui-sr-hook-root
                               :log log :panel panel :mode ,mode)
                "*vui-sr-h*")
     (unwind-protect
         (with-current-buffer "*vui-sr-h*"
           ;; `vui-mount' leaves no render record, and this render does:
           ;; the renders after it patch around LOG when it is the first
           ;; child
           (vui-rerender (vui-get-instance))
           ,@body)
       (vui-sr--kill "*vui-sr-h*"))))

(defun vui-sr-hook--text ()
  "Return the text of the buffer, with its lines joined by \"|\"."
  (replace-regexp-in-string "\n" "|" (buffer-string)))

(defun vui-sr-hook--render-whole ()
  "Render the root again into an empty buffer, return `vui-sr-hook--text'."
  (let ((root (vui-get-instance)))
    (setf (vui-instance-render-record root) nil)
    (vui-rerender root)
    (vui-sr-hook--text)))

(describe "vui-stream rows: renders asked for while a row mounts"
  ;; A row that mounts can ask for renders from its hooks: of the root,
  ;; or of a row it appends.  They wait until the row is in its stream,
  ;; or a render of the root writes the stream without the row and the
  ;; row's text is lost.  A render in progress runs them when it is done.
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "keeps a row that gives another stream its first item (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "r"
                              :hook (lambda ()
                                      (vui-stream-append panel
                                                         (vui-text "p0")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|panel|p0|footer"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[r]|l1|panel|p0|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[r]|l1|panel|p0|footer"))))

      (it (format "keeps a row that appends a row to another stream (%s)" mode)
        ;; The second row renders when the first one is done
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "a"
                              :hook (lambda ()
                                      (vui-stream-append
                                       panel (vui-component 'vui-sr-hook-row
                                                            :label "b")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|panel|p0|[b]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "b2"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|l1|panel|p0|[b2]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a]|l1|panel|p0|[b2]|footer"))))

      (it (format "appends a row from a render of another row (%s)" mode)
        ;; That render is not over when the new row mounts, so the first
        ;; render of the new row waits in the queue: that render runs it
        ;; when it is done
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-update-row :label "a"))
          (let ((once t))
            (vui-update (car (vui-stream-handle-items-rev log))
                        (list :label "a2"
                              :hook (lambda ()
                                      (when once
                                        (setq once nil)
                                        (vui-stream-append
                                         panel (vui-component 'vui-sr-hook-row
                                                              :label "b")))))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a2]|panel|p0|[b]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a2]|panel|p0|[b]|footer"))))

      (it (format "opens a node on a row whose hooks append to its stream (%s)"
                  mode)
        ;; The row's hook appends a row to PANEL, and that row's first
        ;; render opens a node on LOG, after the row
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (let* ((later nil)
                 (node (vui-stream-open
                        log (vui-component
                             'vui-sr-hook-row
                             :label "a"
                             :hook (lambda ()
                                     (vui-stream-append
                                      panel (vui-component
                                             'vui-sr-hook-row
                                             :label "b"
                                             :hook (lambda ()
                                                     (setq later
                                                           (vui-stream-open
                                                            log (vui-text "t")))))))))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[a]|t|panel|p0|[b]|footer"))
            (vui-stream-update node (vui-component 'vui-sr-hook-row :label "a2"))
            (vui-stream-update-last log (vui-text "t2"))
            (vui-stream-update later (vui-text "t3"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|[a2]|t3|panel|p0|[b]|footer"))
            (expect (vui-sr-hook--render-whole)
                    :to-equal (concat x "l0|[a2]|t3|panel|p0|[b]|footer")))))

      (it (format "keeps a row that gives text to a stream of rows with none (%s)"
                  mode)
        ;; Nothing sets apart the items of PANEL, and its only item is a
        ;; row that renders nothing, so the row appended to it brings the
        ;; first text of PANEL with its render
        (vui-sr-hook--with mode ""
          (let ((node (vui-stream-open panel (vui-text "p0"))))
            (vui-stream-append panel (vui-component 'vui-sr-hook-row))
            (vui-stream-remove node))
          (expect (vui-sr-hook--text) :to-equal (concat x "l0|panel|footer"))
          (vui-stream-append
           log (vui-component 'vui-sr-hook-row
                              :label "a"
                              :hook (lambda ()
                                      (vui-stream-append
                                       panel (vui-component 'vui-sr-hook-row
                                                            :label "b")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|panel|[b]|footer"))
          (vui-stream-append log (vui-text "l1"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|[a]|l1|panel|[b]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|[a]|l1|panel|[b]|footer"))))

      (it (format "keeps a row that brings the first text of its stream (%s)"
                  mode)
        ;; Nothing sets apart the items of LOG, and its only item is a row
        ;; that renders nothing
        (let* ((vui-render-delay nil)
               (log (vui-make-stream ""))
               (panel (vui-make-stream)))
          (vui-mount (vui-component 'vui-sr-hook-root
                                    :log log :panel panel :mode mode)
                     "*vui-sr-h*")
          (unwind-protect
              (with-current-buffer "*vui-sr-h*"
                (vui-rerender (vui-get-instance))
                (let ((node (vui-stream-open log (vui-text "l0"))))
                  (vui-stream-append log (vui-component 'vui-sr-hook-row))
                  (vui-stream-remove node))
                (vui-stream-append
                 log (vui-component 'vui-sr-hook-row
                                    :label "a"
                                    :hook (lambda ()
                                            (vui-stream-append
                                             panel (vui-text "p0")))))
                (expect (vui-sr-hook--text)
                        :to-equal (concat x "[a]|panel|p0|footer"))
                (expect (vui-sr-hook--render-whole)
                        :to-equal (concat x "[a]|panel|p0|footer")))
            (vui-sr--kill "*vui-sr-h*")))))))

(defvar vui-sr-above--child-hook nil
  "When non-nil, `vui-sr-above-row' has a child that calls it on mount.")

(vui-defcomponent vui-sr-above-child (hook)
  :on-mount (funcall hook)
  :render (vui-text "(c)"))

(vui-defcomponent vui-sr-above-row (label)
  :render (vui-fragment
           (vui-text (format "[%s]" label))
           (when vui-sr-above--child-hook
             (vui-component 'vui-sr-above-child
                            :hook vui-sr-above--child-hook))))

(describe "vui-stream rows: a row whose hooks write above it"
  ;; The row is in PANEL, and its hooks write into LOG, above it
  (dolist (mode '(first middle))
    (let ((x (if (eq mode 'first) "" "x|")))
      (it (format "keeps its text when a hook of its first render writes above it (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           panel (vui-component 'vui-sr-hook-row
                                :label "r"
                                :hook (lambda ()
                                        (vui-stream-append log (vui-text "t")))))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))))

      (it (format "keeps its text when a later render writes above it (%s)"
                  mode)
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append
           panel (vui-component 'vui-sr-hook-update-row :label "r"))
          (let ((row (car (vui-stream-handle-items-rev panel))))
            (vui-update row
                        (list :label "r2"
                              :hook (lambda ()
                                      (vui-stream-append log (vui-text "t")))))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))
            (vui-update row '(:label "r3"))
            (expect (vui-sr-hook--text)
                    :to-equal (concat x "l0|t|panel|p0|[r3]|footer")))))

      (it (format "keeps its text when its hook writes above it in a render of the root (%s)"
                  mode)
        ;; PANEL is not the first child, so a render of the root writes it
        ;; again, and its row renders again there
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (let ((once t))
            (vui-stream-append
             panel (vui-component 'vui-sr-hook-update-row
                                  :label "r"
                                  :hook (lambda ()
                                          (when once
                                            (setq once nil)
                                            (vui-stream-append
                                             log (vui-text "t")))))))
          (vui-rerender (vui-get-instance))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r]|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))
          (expect (vui-sr-hook--render-whole)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer"))))

      (it (format "keeps its text when a child writes above it in a render of the root (%s)"
                  mode)
        ;; The child of the row mounts when the render of the root writes
        ;; the row again, and its hook runs while the row is being written
        (vui-sr-hook--with mode nil
          (vui-stream-append panel (vui-text "p0"))
          (vui-stream-append panel (vui-component 'vui-sr-above-row :label "r"))
          (let ((vui-sr-above--child-hook
                 (lambda () (vui-stream-append log (vui-text "t")))))
            (vui-rerender (vui-get-instance)))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r](c)|footer"))
          (vui-update (car (vui-stream-handle-items-rev panel)) '(:label "r2"))
          (expect (vui-sr-hook--text)
                  :to-equal (concat x "l0|t|panel|p0|[r2]|footer")))))))

(provide 'vui-stream-rows-test)
;;; vui-stream-rows-test.el ends here

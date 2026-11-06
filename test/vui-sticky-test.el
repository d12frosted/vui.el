;;; vui-sticky-test.el --- Sticky regions -*- lexical-binding: t; -*-

;;; Commentary:

;; Specs for `vui-region' :sticky (issue #305).  A sticky region pins
;; the line it starts on in the header line while the window start is
;; inside the region, past that line.  The region bounds it, as CSS
;; `position: sticky' is bounded by its parent: the next section's
;; heading takes over where a section ends.  The innermost sticky part
;; that holds the window start wins, a table's sticky header included.
;;
;; `vui--sticky-header' takes an optional position standing in for
;; `window-start', which is what these specs drive.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vui-st--at (pos)
  "Pinned text at window-start POS, without the alignment prefix."
  (let ((s (vui--sticky-header pos)))
    (if (string-empty-p s) s (substring-no-properties s 1))))

(defun vui-st--pos-of (text)
  "Position of the line start of the first occurrence of TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (line-beginning-position)))

(defun vui-st--lines (prefix n)
  "Return N text lines PREFIX 1 .. PREFIX N."
  (mapcar (lambda (i) (vui-text (format "%s %d" prefix i)))
          (number-sequence 1 n)))

(defun vui-st--page ()
  "A page: a sticky title over two sticky sections and a footer."
  (vui-region :sticky t
    (vui-vstack
     (vui-text "Page title")
     (vui-text "intro")
     (vui-region :sticky t
       (apply #'vui-vstack (vui-text "Section A") (vui-st--lines "a" 3)))
     (vui-region :sticky t
       (apply #'vui-vstack (vui-text "Section B") (vui-st--lines "b" 3)))
     (vui-text "page end"))))

(describe "vui-region :sticky"
  (it "stores :sticky and :sticky-face in the vnode"
    (let ((node (vui-region :sticky t :sticky-face 'shadow (vui-text "x"))))
      (expect (vui-vnode-region-sticky node) :to-be t)
      (expect (vui-vnode-region-sticky-face node) :to-be 'shadow))
    (expect (vui-vnode-region-sticky (vui-region (vui-text "x"))) :to-be nil))

  (it "renders the buffer exactly like a region that is not sticky"
    (let (plain sticky)
      (with-temp-buffer
        (vui-render (vui-region :face 'bold (vui-vstack (vui-text "h") (vui-text "b"))))
        (setq plain (buffer-string)))
      (with-temp-buffer
        (vui-render (vui-region :sticky t :face 'bold
                      (vui-vstack (vui-text "h") (vui-text "b"))))
        (setq sticky (buffer-string)))
      (expect sticky :to-equal plain)))

  (it "installs an :eval header line format"
    (with-temp-buffer
      (vui-render (vui-region :sticky t (vui-vstack (vui-text "h") (vui-text "b"))))
      (expect header-line-format
              :to-equal '("" (:eval (vui--sticky-header))))))

  (it "pins its first line only while the window start is inside it"
    (with-temp-buffer
      (vui-render (vui-vstack
                   (vui-text "before")
                   (vui-region :sticky t
                     (apply #'vui-vstack (vui-text "Heading") (vui-st--lines "x" 3)))
                   (vui-text "after")))
      ;; Not scrolled past the heading yet
      (expect (vui-st--at (point-min)) :to-equal "")
      (expect (vui-st--at (vui-st--pos-of "Heading")) :to-equal "")
      ;; Inside the region, past its first line
      (expect (vui-st--at (vui-st--pos-of "x 1")) :to-equal "Heading")
      (expect (vui-st--at (vui-st--pos-of "x 3")) :to-equal "Heading")
      ;; Past the region: the end of the region pushes it off
      (expect (vui-st--at (vui-st--pos-of "after")) :to-equal "")))

  (it "hands over from one section's heading to the next, inside a page"
    (with-temp-buffer
      (vui-render (vui-st--page))
      (expect (vui-st--at (point-min)) :to-equal "")
      ;; In the page, before any section: the page title
      (expect (vui-st--at (vui-st--pos-of "intro")) :to-equal "Page title")
      ;; In a section: the innermost wins
      (expect (vui-st--at (vui-st--pos-of "a 2")) :to-equal "Section A")
      (expect (vui-st--at (vui-st--pos-of "b 1")) :to-equal "Section B")
      ;; Past the sections, still in the page
      (expect (vui-st--at (vui-st--pos-of "page end")) :to-equal "Page title")))

  (it "gives way to a sticky table header inside it"
    (with-temp-buffer
      (vui-render (vui-region :sticky t
                    (vui-vstack
                     (vui-text "Section")
                     (vui-table :sticky-header t
                                :columns '((:header "Name" :width 4 :grow t))
                                :rows '(("r1") ("r2")))
                     (vui-text "tail"))))
      (expect (vui-st--at (vui-st--pos-of "r1")) :to-equal "Name")
      (expect (vui-st--at (vui-st--pos-of "tail")) :to-equal "Section")))

  (it "puts :sticky-face on the pinned copy only"
    (with-temp-buffer
      (vui-render (vui-region :sticky t :sticky-face 'shadow
                    (vui-vstack (vui-text "Heading" :face 'bold) (vui-text "body"))))
      (let ((pinned (vui--sticky-header (vui-st--pos-of "body"))))
        (expect (memq 'shadow (ensure-list
                               (get-text-property (1- (length pinned)) 'face pinned)))
                :to-be-truthy)
        ;; The face of the text stays on it
        (expect (memq 'bold (ensure-list
                             (get-text-property (1- (length pinned)) 'face pinned)))
                :to-be-truthy))
      ;; The buffer text does not get it
      (expect (memq 'shadow (ensure-list
                             (get-text-property (vui-st--pos-of "Heading") 'face)))
              :to-be nil)))

  (it "registers once through a measuring container"
    (with-temp-buffer
      (vui-render (vui-box
                   (vui-region :sticky t (vui-vstack (vui-text "h") (vui-text "b")))
                   :width 20))
      (expect (length vui--sticky-registry) :to-equal 1)
      (expect (vui-st--at (vui-st--pos-of "b")) :to-equal "h")))

  (it "registers nothing for a region with no text"
    (with-temp-buffer
      (vui-render (vui-vstack (vui-text "x") (vui-region :sticky t)))
      (expect vui--sticky-registry :to-be nil)))

  (it "keeps its pin when a stream row it starts with renders again"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-st-row (id)
        :state ((open nil))
        :render (vui-vstack (vui-text (format "row %s" id))
                            (when open (vui-text (format "detail %s" id)))))
      (vui-defcomponent vui-st-feed ()
        :render (vui-vstack
                 (vui-text "top")
                 (vui-region :sticky t
                   (vui-vstack (vui-stream stream) (vui-text "tail")))))
      (vui-mount (vui-component 'vui-st-feed) "*vui-st-row*" t)
      (unwind-protect
          (with-current-buffer "*vui-st-row*"
            (vui-stream-append stream (vui-component 'vui-st-row :key "A" :id "A"))
            (vui-stream-append stream (vui-component 'vui-st-row :key "B" :id "B"))
            (let ((row (vui-get-instance-by-key "A")))
              (vui-set-state :open t row)
              (expect (buffer-string) :to-equal "top\nrow A\ndetail A\nrow B\ntail")
              (expect (vui-st--at (vui-st--pos-of "row B")) :to-equal "row A")
              (expect (vui-st--at (vui-st--pos-of "tail")) :to-equal "row A")
              (vui-set-state :open nil row)
              (expect (vui-st--at (vui-st--pos-of "row B")) :to-equal "row A")))
        (kill-buffer "*vui-st-row*"))))

  (it "registers once for a sticky region in a stream row that renders again"
    (let ((vui-render-delay nil)
          (stream (vui-make-stream)))
      (vui-defcomponent vui-st-srow ()
        :state ((n 0))
        :render (vui-region :sticky t
                  (vui-vstack (vui-text "Head") (vui-text (format "n=%d" n)))))
      (vui-defcomponent vui-st-sfeed ()
        :render (vui-stream stream))
      (vui-mount (vui-component 'vui-st-sfeed) "*vui-st-srow*" t)
      (unwind-protect
          (with-current-buffer "*vui-st-srow*"
            (vui-stream-append stream (vui-component 'vui-st-srow :key "r"))
            (let ((row (vui-get-instance-by-key "r")))
              (dotimes (i 3) (vui-set-state :n (1+ i) row)))
            (vui--sticky-prune-registry)
            (expect (length vui--sticky-registry) :to-equal 1)
            (expect (vui-st--at (vui-st--pos-of "n=3")) :to-equal "Head"))
        (kill-buffer "*vui-st-srow*"))))

  (it "lets the inner of two regions on the same line win"
    (with-temp-buffer
      (vui-render (vui-region :sticky t :sticky-face 'bold
                    (vui-vstack
                     (vui-region :sticky t :sticky-face 'italic
                       (vui-vstack (vui-text "Same") (vui-text "in")))
                     (vui-text "out"))))
      (let ((face-at (lambda (text)
                       (let ((s (vui--sticky-header (vui-st--pos-of text))))
                         (ensure-list (get-text-property (1- (length s)) 'face s))))))
        (expect (funcall face-at "in") :to-equal '(italic))
        (expect (funcall face-at "out") :to-equal '(bold)))))

  (it "puts back the header line when a re-render drops it"
    (let ((vui-render-delay nil)
          (set-show nil))
      (vui-defcomponent vui-st-toggle ()
        :state ((show t))
        :render (progn
                  (setq set-show (vui-async-callback (v) (vui-set-state :show v)))
                  (if show
                      (vui-region :sticky t (vui-vstack (vui-text "h") (vui-text "b")))
                    (vui-text "plain"))))
      (vui-mount (vui-component 'vui-st-toggle) "*vui-st-drop*" t)
      (unwind-protect
          (with-current-buffer "*vui-st-drop*"
            (expect header-line-format :to-equal '("" (:eval (vui--sticky-header))))
            (funcall set-show nil)
            (expect (buffer-string) :to-equal "plain")
            ;; What `vui-mode' had before the sticky line replaced it
            (expect header-line-format :to-be nil))
        (kill-buffer "*vui-st-drop*")))))

;;; vui-sticky-test.el ends here

;;; vui-agent-chat-test.el --- Agent-chat streaming seam invariants -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Correctness invariants for the agent-chat example (docs/examples/
;; 13-agent-chat.el), the running case for the `vui-stream' imperative
;; append primitive (issue #82, dir 5).
;;
;; The layout is the seam that stresses imperative-inside-declarative: a
;; transcript that grows ABOVE a persistent box whose input field must
;; stay present and correct no matter how many messages arrive.
;;
;; These run against the DECLARATIVE BASELINE (no `vui-stream' yet), so
;; they pass today.  Their job is twofold:
;;   1. pin the baseline behavior (box present, roles drawn distinctly);
;;   2. establish the ORACLE: growing by `vui-update' is byte-identical
;;      to a fresh mount of the final message list.  When `vui-stream'
;;      lands, the stream-built buffer must match this same oracle
;;      (`vui-agent--string'), which is the differential check.

;;; Code:

(require 'buttercup)
(require 'vui)
(require 'cl-lib)

;; The example app lives under docs/examples; put it on the load path by
;; finding the project root (the directory holding vui.el) from here.
(let* ((here (or load-file-name buffer-file-name default-directory))
       (root (locate-dominating-file here "vui.el")))
  (when root
    (add-to-list 'load-path (expand-file-name "docs/examples" root))))
(require '13-agent-chat)

(defun vui-agent--kill (buf)
  "Kill BUF, tolerating field widgets (see the unmount caveat)."
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-modification-hooks t))
        (kill-buffer buf)))))

(defun vui-agent--messages (n)
  "Return N demo messages cycling through the three roles."
  (cl-loop for i from 1 to n
           collect (list :id i
                         :role (nth (mod i 3) '(user agent tool))
                         :text (format "m%d" i))))

(defun vui-agent--string (messages queue status)
  "Buffer string of a FRESH mount of `vui-agent-chat'.
This is the oracle: the canonical rendering of MESSAGES/QUEUE/STATUS
that any incremental path must reproduce byte-for-byte."
  (let* ((buf "*vui-agent-oracle*")
         (inst (vui-mount (vui-component 'vui-agent-chat
                            :messages messages :queue queue :status status)
                          buf)))
    (ignore inst)
    (prog1 (with-current-buffer buf (buffer-string))
      (vui-agent--kill buf))))

(describe "agent-chat baseline: messages drawn per role"
  (it "renders user, agent, and tool messages distinctly"
    (let* ((vui-render-delay nil)
           (s (vui-agent--string
               '((:id 1 :role user  :text "hi")
                 (:id 2 :role agent :text "hello")
                 (:id 3 :role tool  :text "ran"))
               0 "idle")))
      (expect s :to-match "you> hi")
      (expect s :to-match "ai>  hello")
      (expect s :to-match "\\[tool\\] ran"))))

(describe "agent-chat baseline: the box is always present"
  (it "renders status, queue, and an input field below the transcript"
    (let* ((vui-render-delay nil)
           (buf "*vui-agent-box*")
           (inst (vui-mount (vui-component 'vui-agent-chat
                              :messages (vui-agent--messages 5)
                              :queue 3 :status "thinking")
                            buf)))
      (ignore inst)
      (unwind-protect
          (with-current-buffer buf
            (expect (buffer-string) :to-match "status: thinking")
            (expect (buffer-string) :to-match "queue: 3 pending")
            ;; the field widget really exists (placeholder is an overlay,
            ;; not buffer text, so assert on the widget list instead)
            (expect widget-field-list :not :to-be nil))
        (vui-agent--kill buf))))

  (it "keeps the box present and correct after many appends"
    (let* ((vui-render-delay nil)
           (buf "*vui-agent-grow-box*")
           (base (vui-agent--messages 1))
           (inst (vui-mount (vui-component 'vui-agent-chat
                              :messages base :queue 0 :status "idle")
                            buf)))
      (ignore inst)
      (unwind-protect
          (progn
            ;; stream 200 messages in, one at a time
            (let ((msgs base))
              (dotimes (i 200)
                (setq msgs (append msgs
                                   (list (list :id (+ 2 i)
                                               :role 'agent
                                               :text (format "x%d" i)))))
                (vui-update inst (list :messages msgs :queue 0 :status "idle"))))
            (with-current-buffer buf
              ;; box still there, still has its field, after 200 appends
              (expect (buffer-string) :to-match "status: idle")
              (expect (buffer-string) :to-match "queue: 0 pending")
              (expect widget-field-list :not :to-be nil)))
        (vui-agent--kill buf)))))

(describe "agent-chat baseline: the growth oracle"
  ;; The load-bearing invariant for vui-stream: growing the transcript
  ;; by `vui-update' must produce exactly what a fresh mount of the final
  ;; list produces.  This pins order-independence and gives the stream
  ;; path its differential target.
  (it "grows byte-identically to a fresh mount (single append)"
    (let* ((vui-render-delay nil)
           (base (vui-agent--messages 10))
           (grown (append base (list (list :id 11 :role 'user :text "m11"))))
           (buf "*vui-agent-grow1*")
           (inst (vui-mount (vui-component 'vui-agent-chat
                              :messages base :queue 0 :status "idle")
                            buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (vui-update inst (list :messages grown :queue 0 :status "idle"))
            (expect (with-current-buffer buf (buffer-string))
                    :to-equal (vui-agent--string grown 0 "idle")))
        (vui-agent--kill buf))))

  (it "grows byte-identically across a long stream"
    (let* ((vui-render-delay nil)
           (buf "*vui-agent-grow-n*")
           (msgs (vui-agent--messages 1))
           (inst (vui-mount (vui-component 'vui-agent-chat
                              :messages msgs :queue 2 :status "busy")
                            buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (dotimes (i 50)
              (setq msgs (append msgs
                                 (list (list :id (+ 2 i) :role 'tool
                                             :text (format "t%d" i)))))
              (vui-update inst (list :messages msgs :queue 2 :status "busy")))
            (expect (with-current-buffer buf (buffer-string))
                    :to-equal (vui-agent--string msgs 2 "busy")))
        (vui-agent--kill buf)))))

(describe "agent-chat: the streaming transcript matches the declarative UI"
  ;; The vui-stream version must render byte-identically to the wholesale
  ;; vui-agent-chat oracle - same messages, same box.
  (it "appends messages via vui-stream to the same buffer as a fresh mount"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (msgs (vui-agent--messages 25))
           (buf "*vui-agent-stream*")
           (inst (vui-mount (vui-component 'vui-agent-chat-stream
                              :stream s :queue 2 :status "busy")
                            buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (dolist (m msgs)
              (vui-stream-append
               s (vui-chat-message-vnode (plist-get m :role) (plist-get m :text))))
            (expect (with-current-buffer buf (buffer-string))
                    :to-equal (vui-agent--string msgs 2 "busy")))
        (vui-agent--kill buf))))

  (it "keeps the box (field + status) correct as the stream grows"
    (let* ((vui-render-delay nil)
           (s (vui-make-stream))
           (buf "*vui-agent-stream-box*")
           (inst (vui-mount (vui-component 'vui-agent-chat-stream
                              :stream s :queue 0 :status "idle")
                            buf)))
      (ignore inst)
      (unwind-protect
          (progn
            (dotimes (i 150)
              (vui-stream-append s (vui-chat-message-vnode 'agent (format "m%d" i))))
            (with-current-buffer buf
              (expect (buffer-string) :to-match "status: idle")
              (expect (buffer-string) :to-match "queue: 0 pending")
              (expect widget-field-list :not :to-be nil)))
        (vui-agent--kill buf)))))

(provide 'vui-agent-chat-test)
;;; vui-agent-chat-test.el ends here

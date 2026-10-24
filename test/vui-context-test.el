;;; vui-context-test.el --- Context tests for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for vui.el context system: providers, consumers, use-context.

;;; Code:

(require 'buttercup)
(require 'vui)

(describe "context"
  (it "defines context with defcontext"
    (vui-defcontext test-ctx 'default-value "A test context.")
    (expect (boundp 'test-ctx-context) :to-be-truthy)
    (expect (fboundp 'test-ctx-provider) :to-be-truthy)
    (expect (fboundp 'use-test-ctx) :to-be-truthy)
    (expect (vui-context-default-value test-ctx-context) :to-equal 'default-value))

  (it "provides context to a list of children given as one argument"
    (vui-defcontext list-ctx 'light)
    (vui-defcomponent list-ctx-consumer (label)
      :render (vui-text (format "%s:%s " label (use-list-ctx))))
    (with-temp-buffer
      (vui-render (list-ctx-provider 'dark
                    (cl-loop for label in '("a" "b")
                             collect (vui-component 'list-ctx-consumer
                                                    :label label))))
      (expect (buffer-string) :to-equal "a:dark b:dark ")))

  (it "consumes context via vui-use-context"
    (vui-defcontext uc-theme 'light)
    (let ((inside nil)
          (outside nil))
      (vui-defcomponent uc-consumer ()
        :render (progn
                  (setq inside (vui-use-context uc-theme-context))
                  (vui-text "x")))
      (vui-defcomponent uc-app ()
        :render (uc-theme-provider 'dark
                  (vui-component 'uc-consumer)))
      (vui-defcomponent uc-bare ()
        :render (progn
                  (setq outside (vui-use-context uc-theme-context))
                  (vui-text "y")))
      (vui-mount (vui-component 'uc-app) "*test-uc1*")
      (vui-mount (vui-component 'uc-bare) "*test-uc2*")
      (unwind-protect
          (progn
            ;; Provided value inside a provider
            (expect inside :to-equal 'dark)
            ;; Default value without a provider
            (expect outside :to-equal 'light))
        (kill-buffer "*test-uc1*")
        (kill-buffer "*test-uc2*"))))

  (it "vui-use-context matches the generated use-NAME consumer"
    (vui-defcontext uc-pair 'fallback)
    (let ((via-generic nil)
          (via-generated nil))
      (vui-defcomponent uc-pair-consumer ()
        :render (progn
                  (setq via-generic (vui-use-context uc-pair-context))
                  (setq via-generated (use-uc-pair))
                  (vui-text "z")))
      (vui-defcomponent uc-pair-app ()
        :render (uc-pair-provider 42
                  (vui-component 'uc-pair-consumer)))
      (vui-mount (vui-component 'uc-pair-app) "*test-uc3*")
      (unwind-protect
          (progn
            (expect via-generic :to-equal 42)
            (expect via-generated :to-equal 42))
        (kill-buffer "*test-uc3*"))))

  (it "returns default value when no provider"
    (vui-defcontext theme 'light)
    (let ((captured-theme nil))
      (vui-defcomponent theme-consumer ()
        :render (progn
                  (setq captured-theme (use-theme))
                  (vui-text "test")))
      (let ((instance (vui-mount (vui-component 'theme-consumer) "*test-ctx1*")))
        (unwind-protect
            (expect captured-theme :to-equal 'light)
          (kill-buffer "*test-ctx1*")))))

  (it "provides value to children"
    (vui-defcontext theme 'light)
    (let ((captured-theme nil))
      (vui-defcomponent themed-button ()
        :render (progn
                  (setq captured-theme (use-theme))
                  (vui-text (format "theme: %s" captured-theme))))
      (vui-defcomponent app ()
        :render (theme-provider 'dark
                                (vui-component 'themed-button)))
      (let ((instance (vui-mount (vui-component 'app) "*test-ctx2*")))
        (unwind-protect
            (progn
              (expect captured-theme :to-equal 'dark)
              (expect (buffer-string) :to-equal "theme: dark"))
          (kill-buffer "*test-ctx2*")))))

  (it "allows nested providers with different values"
    (vui-defcontext indent-level 0)
    (let ((captured-levels nil))
      (vui-defcomponent level-display ()
        :render (progn
                  (push (use-indent-level) captured-levels)
                  (vui-text (number-to-string (use-indent-level)))))
      (vui-defcomponent nested-providers ()
        :render (vui-fragment
                 (indent-level-provider 1
                                        (vui-component 'level-display)
                                        (indent-level-provider 2
                                                               (vui-component 'level-display)))))
      (let ((instance (vui-mount (vui-component 'nested-providers) "*test-ctx3*")))
        (unwind-protect
            (progn
              ;; Both levels should have been captured (in reverse order)
              (expect (length captured-levels) :to-equal 2)
              (expect (nth 0 captured-levels) :to-equal 2)  ; inner
              (expect (nth 1 captured-levels) :to-equal 1)) ; outer
          (kill-buffer "*test-ctx3*")))))

  (it "supports multiple contexts"
    (vui-defcontext user-name "anonymous")
    (vui-defcontext user-role 'guest)
    (let ((captured-name nil)
          (captured-role nil))
      (vui-defcomponent multi-ctx-consumer ()
        :render (progn
                  (setq captured-name (use-user-name))
                  (setq captured-role (use-user-role))
                  (vui-text "test")))
      (vui-defcomponent multi-ctx-provider ()
        :render (user-name-provider "alice"
                                    (user-role-provider 'admin
                                                        (vui-component 'multi-ctx-consumer))))
      (let ((instance (vui-mount (vui-component 'multi-ctx-provider) "*test-ctx4*")))
        (unwind-protect
            (progn
              (expect captured-name :to-equal "alice")
              (expect captured-role :to-equal 'admin))
          (kill-buffer "*test-ctx4*"))))))


(vui-defcontext vui-context-test-theme 'light
  "A context for the specs of trees mounted from a hook.")

(defvar vui-context-test--side-root nil
  "The root that `vui-context-test-mounting-host' mounts.")

(vui-defcomponent vui-context-test-theme-reader ()
  "The value of `vui-context-test-theme' as text."
  :render (vui-text (format "theme %s" (vui-use-context
                                        vui-context-test-theme-context))))

(vui-defcomponent vui-context-test-mounting-host ()
  "Mounts a theme reader in a buffer of its own from its `:on-mount'."
  :on-mount (setq vui-context-test--side-root
                  (save-window-excursion
                    (vui-mount (vui-component 'vui-context-test-theme-reader)
                               "*test-ctx-side*")))
  :render (vui-text "host"))

(vui-defcomponent vui-context-test-provided-host ()
  "A `dark' provider around a `vui-context-test-mounting-host' and a
theme reader."
  :render (vui-context-test-theme-provider 'dark
            (vui-vstack (vui-component 'vui-context-test-mounting-host)
                        (vui-component 'vui-context-test-theme-reader))))

(vui-defcomponent vui-context-test-hook-runner (hook)
  "Calls HOOK from its `:on-mount'."
  :on-mount (funcall hook)
  :render (vui-text "runner"))

(describe "a tree mounted from a hook"
  (it "does not see the context values of the tree it is mounted from"
    (let ((vui-render-delay nil))
      (unwind-protect
          (progn
            (save-window-excursion
              (vui-mount (vui-component 'vui-context-test-provided-host)
                         "*test-ctx-host*"))
            (expect (with-current-buffer "*test-ctx-side*" (buffer-string))
                    :to-equal "theme light")
            ;; The same after a render of its own
            (vui-rerender vui-context-test--side-root)
            (expect (with-current-buffer "*test-ctx-side*" (buffer-string))
                    :to-equal "theme light")
            ;; And the tree it is mounted from keeps its own
            (expect (with-current-buffer "*test-ctx-host*" (buffer-string))
                    :to-equal "host\ntheme dark"))
        (dolist (buffer '("*test-ctx-side*" "*test-ctx-host*"))
          (when (get-buffer buffer)
            (kill-buffer buffer))))))

  ;; The hooks of a tree that `vui-render' renders run outside of any
  ;; render, so what they render is rendered right away
  (it "does not see the context values of a tree that renders it from a hook"
    (let ((vui-render-delay nil)
          (root nil))
      (unwind-protect
          (progn
            (setq root (save-window-excursion
                         (vui-mount (vui-component 'vui-context-test-theme-reader)
                                    "*test-ctx-side*")))
            (with-temp-buffer
              (vui-render (vui-context-test-theme-provider 'dark
                            (vui-component 'vui-context-test-hook-runner
                                           :hook (lambda () (vui-rerender root))))))
            (expect (with-current-buffer "*test-ctx-side*" (buffer-string))
                    :to-equal "theme light"))
        (when (get-buffer "*test-ctx-side*")
          (kill-buffer "*test-ctx-side*")))))

  (it "does not see the context values of a tree that mounts it inline from a hook"
    (let ((vui-render-delay nil)
          (instance nil))
      (with-temp-buffer
        (insert "host\n")
        (let ((host (current-buffer)))
          (with-temp-buffer
            (vui-render (vui-context-test-theme-provider 'dark
                          (vui-component
                           'vui-context-test-hook-runner
                           :hook (lambda ()
                                   (with-current-buffer host
                                     (setq instance
                                           (vui-mount-inline
                                            (vui-component
                                             'vui-context-test-theme-reader)
                                            (point-max))))))))))
        (pcase-let ((`(,start . ,end) (vui--inline-bounds instance)))
          (expect (buffer-substring-no-properties start end)
                  :to-equal "theme light"))))))

;;; vui-context-test.el ends here

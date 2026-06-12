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


;;; vui-context-test.el ends here

;;; -*- lexical-binding: t -*-

(leaf evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-echo-state nil)
  :config (evil-mode +1))

(leaf modus-themes
  :ensure t
  :config (load-theme 'modus-vivendi t))

(leaf vertico
  :ensure t
  :config (vertico-mode +1))

(leaf orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

(leaf mood-line
  :ensure t
  :config (mood-line-mode +1))

(leaf git-gutter
  :ensure t
  :init (setq git-gutter:modified-sign "~")
  :config (global-git-gutter-mode +1))

(leaf haskell-mode
  :ensure t)

;; Disable user interface elements
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; Disable startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

;; Disable {beginning,end} of buffer messages
;; (https://emacs.stackexchange.com/a/20039)
(defun quieter-command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-buffer
				end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'quieter-command-error-function)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Disable clutter files
(setq custom-file "/dev/null")
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; Shorten yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks when opening files under version control
(setq vc-follow-symlinks t)

;; Indent with 2 spaces
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; Enable line numbers
(global-display-line-numbers-mode +1)
(setq display-line-numbers-width 3)

;; Show ruler at 80 columns
(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

;; Make the mouse wheel scroll 3 lines at a time
(setq scroll-conservatively 10000)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))
(unless (display-graphic-p)
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

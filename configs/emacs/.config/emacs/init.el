(leaf evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-echo-state nil)
  :config (evil-mode +1))

(leaf modus-themes
  :ensure t
  :config (load-theme 'modus-vivendi t))

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

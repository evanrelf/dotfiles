;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config (leaf-keywords-init)))
;; </leaf-install-code>

(leaf evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
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

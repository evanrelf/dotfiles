;; vim: ft=lisp

;; How to byte-compile ~/.emacs.d:
;; (byte-recompile-directory "~/.emacs.d/" 0 t)

;; PACKAGES
;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Appearance
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(use-package doom-modeline
  :config
  (doom-modeline-init)
  (setq doom-modeline-height 20))
;; Evil
(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-terminal-cursor-changer
  :init
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar)
  :config
  (unless
    (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))
(use-package evil-collection
  :config
  (evil-collection-init))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package evil-exchange
  :config
  (evil-exchange-install))
(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
(use-package evil-vimish-fold
  :config
  (evil-vimish-fold-mode 1))
(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.100)
  (setq evil-goggles-blocking-duration 0.100))
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))
(use-package evil-magit)
;; Git
(use-package magit)
;; Intelligence
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-elm))
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))
(use-package format-all)
(use-package ivy
  :config
  (ivy-mode 1))
(use-package counsel
  :config
  (counsel-mode 1))
;; Languages
(use-package haskell-mode)
(use-package intero)
  ;; :config
  ;; (intero-global-mode 1))
;; (use-package dante
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   :config
;;   (add-hook 'dante-mode-hook
;; 	    '(lambda () (flycheck-add-next-checker 'haskell-dante
;; 						   '(warning . haskell-hlint)))))
(use-package purescript-mode
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
(use-package company-irony)
(use-package elm-mode
  :config
  (setq elm-format-on-save t))
(use-package markdown-mode)
(use-package dhall-mode)
(use-package yaml-mode
  :config (add-hook 'yaml-mode-hook
                    '(lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
;; Other
(use-package general
  :config
  (general-evil-setup t))
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))
(use-package simpleclip
  :config
  (simpleclip-mode t))
(use-package ws-butler
  :config
  (ws-butler-global-mode))
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; SETTINGS
;; GUI font
(setq default-frame-alist '((font . "PragmataPro Liga-16")))
;; Disable bold font
(set-face-bold 'bold nil)
;; Enable line numbers
(global-display-line-numbers-mode)
;; Disable menu bar
(menu-bar-mode -1)
;; Disable tool bar
(tool-bar-mode -1)
;; Disable scroll bar
;; (scroll-bar-mode -1)
;; (horizontal-scroll-bar-mode -1)
;; Disable cursor blink
(blink-cursor-mode 0)
;; Disable audio bell
(setq ring-bell-function 'ignore)
;; Colored GUI titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; Increase memory threshold before garbage collection
(setq gc-cons-threshold 20000000)
;; Follow symlinks when opening files under version control
(setq vc-follow-symlinks t)
;; Make files with a shebang executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; Always apply syntax highlighting
(global-font-lock-mode t)
;; Sentences shouldn't have to end with two spaces
(setq sentence-end-double-space nil)
;; Use y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; Disable startup message
(setq inhibit-startup-message t)
;; Disable scratch buffer on startup
(setq initial-scratch-message nil)
;; Auto reload file when modified externally
(global-auto-revert-mode t)
;; Disable startup message
(setq inhibit-startup-echo-area-message "evanrelf")
(setq inhibit-startup-message t)
;; Disable backup files
(setq make-backup-files nil)
;; Disable auto-save files
(setq auto-save-default nil)
;; Stop evil from using the system l)
;; Indent with 2 spaces
(setq tab-width 2)
(setq indent-tabs-mode nil)
;; Enable mouse support in terminal
(xterm-mouse-mode 1)
(setq scroll-step 3)
;; (unless window-system
;;   (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
;;   (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
(unless window-system
  (global-set-key (kbd "<mouse-4>")
		  (lambda ()
		    (interactive)
		    (scroll-down-line)
		    (scroll-down-line)
		    (scroll-down-line)
		    ))
  (global-set-key (kbd "<mouse-5>")
		  (lambda ()
		    (interactive)
		    (scroll-up-line)
		    (scroll-up-line)
		    (scroll-up-line)
		    )))
;; Control mini window sizing
(setq resize-mini-windows t)
(setq max-mini-window-height 10)
(setq minibuffer-scroll-window t)
;; Scroll line-by-line
(setq scroll-step 1)
(setq scroll-conservatively 10000)
;; Maximize new GUI frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Disable customizations
(setq custom-file "/dev/null")

;; MAPPINGS
(mmap
  ;; ";" 'evil-ex
  ;; ":" 'evil-repeat-find-char
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

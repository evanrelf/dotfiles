;; -*- lexical-binding: t; -*-

;; Increase garbage collection threshold from default 8 MB
(setq gc-cons-threshold 32000000) ;; 32 MB

;; Enable messages to detect thrashing
(setq garbage-collection-messages t)

;; Increase recursion limit
(setq max-lisp-eval-depth 2000)

;; Maximize GUI frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable superfluous chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(fringe-mode -1)

;; Enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Disable customizations
(setq custom-file "/dev/null")

;; Font
(setq default-frame-alist '((font . "PragmataPro Liga-16")))

;; Smaller font size adjustment increments
(setq text-scale-mode-step 1.1)

;; UTF-8
(prefer-coding-system 'utf-8)

;; Display line numbers
(global-display-line-numbers-mode)

;; Always apply syntax highlighting
(global-font-lock-mode t)

;; Highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-delay 0)

;; Disable startup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "evanrelf")

;; Disable clutter files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Start in org-mode
(setq initial-major-mode 'org-mode)

;; Use y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks when opening files under version control
(setq vc-follow-symlinks t)

;; Auto reload file when modified externally
(global-auto-revert-mode t)

;; Make files with a shebang executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Sentences shouldn't have to end with two spaces
(setq sentence-end-double-space nil)

;; Indent with 2 spaces
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; Control mini window sizing
(setq resize-mini-windows t)
(setq max-mini-window-height 10)
(setq minibuffer-scroll-window t)

;; Scroll line-by-line
(setq scroll-conservatively 10000)

;; Scroll 3 lines in GUI
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent
	 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Garbage collector magic hack
(use-package gcmh
  :config (gcmh-mode t))

;; Easy keybindings
(use-package general
  :config (general-evil-setup))

;; Themes
;; (use-package doom-themes)
;; (use-package base16-theme)
;; (use-package color-theme-sanityinc-tomorrow)
;; (use-package leuven-theme)
;; (load-theme 'doom-challenger-deep t)
(use-package modus-operandi-theme)
(load-theme 'modus-operandi t)

;; Modeline
(use-package mood-line
  :config (mood-line-mode))

;; Prevent plugins from polluting ~/.emacs.d/ or my $HOME
(use-package no-littering)

;; Get PATH variable from shell
(use-package exec-path-from-shell
  :after no-littering
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Vim keybindings
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config (evil-mode t))

;; Evil extras
(use-package evil-collection
  :after evil
  :config (evil-collection-init))
(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode t))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))
(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))
(use-package evil-goggles
  :after evil
  :init (setq evil-goggles-duration 0.05)
  :config (evil-goggles-mode))
(use-package evil-escape
  :after evil
  :config (evil-escape-mode))

;; Auto-complete
(use-package company
  :init (setq company-idle-delay 0.1)
  :hook (after-init . global-company-mode))

;; Automatically insert and manage closing pairs
(use-package smartparens
  :config (smartparens-global-mode t))
(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Display errors
(use-package flycheck
  :init
  (setq flycheck-disabled-checkers
	'(emacs-lisp
	  emacs-lisp-checkdoc
	  haskell-ghc
	  haskell-stack-ghc
	  racket))
  ;; Only run flycheck on initial load and file saving
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config (global-flycheck-mode))
(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; Terminal
(use-package vterm
  :init
  (setq vterm-shell "fish")
  (setq vterm-max-scrollback 10000))
(use-package multi-vterm)

;; Amazing Git porcelain
(use-package magit
  :defer 5)
(use-package evil-magit
  :after (evil magit))

;; Projectile
(use-package projectile
  :init (setq projectile-completion-system 'ivy)
  :config (projectile-mode t))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Ivy
(use-package ivy
  :config (ivy-mode t))
(use-package ivy-rich
  :config (ivy-rich-mode t))

;; Counsel
(use-package counsel
  :config (counsel-mode t))

;; Sort and filter candidates
(use-package prescient
  :config (prescient-persist-mode))
(use-package ivy-prescient
  :after (prescient counsel ivy)
  :config (ivy-prescient-mode))
(use-package company-prescient
  :after (prescient company)
  :config (company-prescient-mode))

;; ripgrep
(use-package ripgrep)
(use-package deadgrep)

;; Display current match and show count when searching
(use-package anzu
  :config
  (global-anzu-mode t)
  (anzu-mode t))
(use-package evil-anzu
  :after (evil anzu))

;; Show available keybindings after a delay
(use-package which-key
  :init (setq which-key-idle-delay 0.2)
  :config (which-key-mode))

;; Zero-config jump-to-definition
(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (dumb-jump-mode))

;; Show Git changes in gutter
(use-package git-gutter
  :config
  (custom-set-variables '(git-gutter:modified-sign "~"))
  (global-git-gutter-mode t))

;; Stop the Emacs and system clipboards from mixing
(use-package simpleclip
  :config (simpleclip-mode t))

;; Clean up whitespace on lines I've modified
(use-package ws-butler
  :config (ws-butler-global-mode))

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :config (global-hl-todo-mode t))

;; Languages
(use-package haskell-mode
  :commands (haskell-mode)
  :hook
  (haskell-mode . (lambda ()
		    (setq-local paragraph-separate "[ \t\f]*$")
		    (setq-local paragraph-start "\f\\|[ \t]*$"))))
(use-package nix-mode
  :commands (nix-mode))
(use-package rust-mode
  :commands (rust-mode))
(use-package purescript-mode
  :commands (purescript-mode)
  :hook (purescript-mode . turn-on-purescript-indentation))
(use-package dhall-mode
  :commands (dhall-mode))
(use-package racket-mode
  :commands (racket-mode))
(use-package protobuf-mode
  :commands (protobuf-mode))
(use-package dockerfile-mode
  :commands (dockerfile-mode))
(use-package web-mode
  :commands (web-mode))
(use-package lua-mode
  :commands (lua-mode))
(use-package fish-mode
  :commands (fish-mode))
(use-package markdown-mode
  :commands (markdown-mode))
(use-package yaml-mode
  :commands (yaml-mode)
  :hook (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Adjust text scale
(defun text-scale-reset ()
  "Reset the text scale"
  (interactive)
  (text-scale-adjust 0))
(general-def
  "s-=" 'text-scale-increase
  "s--" 'text-scale-decrease
  "s-0" 'text-scale-reset)

;; Evil
(general-def
  :states '(normal visual)
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;; Leader
(general-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal visual insert emacs)
  :keymaps 'override
  "" '(nil :which-key "leader")
  "ESC" '(evil-escape :which-key t)
  "C-g" '(evil-escape :which-key t)
  "SPC" '(counsel-M-x :which-key "M-x")

  "w" '(:ignore t :which-key "window")
  "w C-g" '(evil-escape :which-key t)
  "w ESC" '(evil-escape :which-key t)
  "w -" '(evil-window-split :which-key "split")
  "w \\" '(evil-window-vsplit :which-key "vsplit")
  "w n" '(evil-window-next :which-key "next")
  "w p" '(evil-window-prev :which-key "previous")
  "w h" '(evil-window-left :which-key "left")
  "w j" '(evil-window-down :which-key "down")
  "w k" '(evil-window-up :which-key "up")
  "w l" '(evil-window-right :which-key "right")
  "w d" '(evil-window-delete :which-key "delete")

  "b" '(:ignore t :which-key "buffer")
  "b C-g" '(evil-escape :which-key t)
  "b ESC" '(evil-escape :which-key t)
  "b l" '(ibuffer :which-key "list")
  "b s" '(counsel-ibuffer :which-key "switch")
  "b n" '(next-buffer :which-key "next")
  "b p" '(previous-buffer :which-key "previous")
  "b d" '(evil-delete-buffer :which-key "delete")

  "f" '(:ignore t :which-key "file")
  "f C-g" '(evil-escape :which-key t)
  "f ESC" '(evil-escape :which-key t)
  "f s" '(save-buffer :which-key "save")
  "f f" '(counsel-find-file :which-key "find")

  "t" '(:ignore t :which-key "terminal")
  "t C-g" '(evil-escape :which-key t)
  "t ESC" '(evil-escape :which-key t)
  "t c" '(multi-vterm :which-key "create")
  "t t" '(multi-vterm-dedicated-toggle :which-key "toggle")
  "t n" '(multi-vterm-next :which-key "next")
  "t p" '(multi-vterm-prev :which-key "previous")

  "p" '(:ignore t :which-key "project")
  "p C-g" '(evil-escape :which-key t)
  "p ESC" '(evil-escape :which-key t)
  "p s" '(counsel-projectile-switch-project :which-key "switch")
  "p f" '(counsel-projectile-find-file :which-key "find file")
  "p d" '(projectile-dired :which-key "dired")
  "p t" '(multi-vterm-projectile :which-key "terminal")

  "p b" '(:ignore t :which-key "buffer")
  "p b C-g" '(evil-escape :which-key t)
  "p b ESC" '(evil-escape :which-key t)
  "p b l" '(projectile-ibuffer :which-key "list")
  "p b s" '(counsel-projectile-switch-buffer :which-key "switch")
  "p b n" '(projectile-next-project-buffer :which-key "next")
  "p b p" '(projectile-previous-project-buffer :which-key "previous")

  "g" '(:ignore t :which-key "git")
  "g C-g" '(evil-escape :which-key t)
  "g ESC" '(evil-escape :which-key t)
  "g s" '(magit-status :which-key "status")

  "h" '(:ignore t :which-key "help")
  "h C-g" '(evil-escape :which-key t)
  "h ESC" '(evil-escape :which-key t)
  "h v" '(describe-variable :which-key "variable")
  "h f" '(describe-function :which-key "function")
  "h k" '(describe-key :which-key "key")
  "h m" '(describe-mode :which-key "mode")
  "h p" '(describe-package :which-key "package")

  "," '((lambda () (interactive) (evil-edit "~/.emacs.d/init.el")) :which-key "edit config"))

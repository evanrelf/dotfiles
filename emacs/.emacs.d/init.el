;; -*- lexical-binding: t; -*-
;; vim: ft=lisp

;; How to byte-compile ~/.emacs.d:
;; (byte-recompile-directory "~/.emacs.d/" 0 t)

;; Increase garbage collection threshold from default 8 MB
(setq-default gc-cons-threshold 32000000) ;; 32 MB

;; Enable messages to detect thrashing
(setq-default garbage-collection-messages t)

;; Increase recursion limit
(setq-default max-lisp-eval-depth 2000)

;; Maximize GUI frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Use dark window chrome in GUI
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Disable superfluous chrome in GUI
(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; Enable mouse support in terminal
(xterm-mouse-mode t)
(setq-default scroll-step 3)
(unless window-system
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

;; Disable audio bell
(setq-default ring-bell-function 'ignore)

;; Disable customizations
(setq-default custom-file "/dev/null")

;; Font
(ignore-errors (set-frame-font "PragmataPro Liga-16"))
(set-face-bold 'bold nil)

;; Smaller font size adjustment increments
(setq-default text-scale-mode-step 1.1)

;; UTF-8
(prefer-coding-system 'utf-8)

;; Display line numbers
(global-display-line-numbers-mode)

;; Always apply syntax highlighting
(global-font-lock-mode t)

;; Disable startup messages
(setq-default initial-scratch-message nil)
(setq-default inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message "evanrelf")

;; Disable clutter files
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Use y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks when opening files under version control
(setq-default vc-follow-symlinks t)

;; Auto reload file when modified externally
(global-auto-revert-mode t)

;; Make files with a shebang executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Sentences shouldn't have to end with two spaces
(setq-default sentence-end-double-space nil)

;; Indent with 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Control mini window sizing
(setq-default resize-mini-windows t)
(setq-default max-mini-window-height 10)
(setq-default minibuffer-scroll-window t)

;; Scroll line-by-line
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)

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
(setq-default straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Other themes
;; (use-package modus-operandi-theme
;;   :config
;;   (load-theme 'modus-operandi t))
;; (use-package modus-vivendi-theme
;;   :config
;;   (load-theme 'modus-vivendi t))

;; Modeline
(use-package mood-line
  :config
  (mood-line-mode))

;; Tabs
(use-package centaur-tabs
  :after all-the-icons
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (setq-default centaur-tabs-gray-out-icons 'buffer)
  (setq-default centaur-tabs-style "bar")
  (setq-default centaur-tabs-set-bar 'over)
  (setq-default centaur-tabs-height 20)
  (setq-default centaur-tabs-set-modified-marker t)
  (setq-default centaur-tabs-modified-marker "●"))

;; Icons
(use-package all-the-icons)

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
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; Evil extras
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-vimish-fold
  :after evil
  :config
  (evil-vimish-fold-mode t))
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode t))
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq-default evil-goggles-duration 0.05))
(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode))

;; Auto-complete
(use-package company
  :config
  (setq-default company-idle-delay 0.1)
  :hook
  (after-init . global-company-mode))

;; Automatically insert and manage closing pairs
(use-package smartparens
  :config
  (smartparens-global-mode t))
(use-package evil-smartparens
  :after (evil smartparens)
  :hook
  (smartparens-enabled . evil-smartparens-mode))

;; Highlight pairs
(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

;; Display errors
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  ;; Only run flycheck on initial load and file saving
  (setq-default flymake-no-changes-timeout nil)
  (setq-default flymake-start-syntax-check-on-newline nil)
  (setq-default flycheck-check-syntax-automatically '(save mode-enabled))
  :hook
  (after-init . global-flycheck-mode))
(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; Amazing Git porcelain
(use-package magit
  :defer 2)
(use-package evil-magit
  :after (evil magit))

;; Projectile
(use-package projectile
  :config
  (projectile-mode t))

;; Ivy
(use-package ivy
  :config
  (ivy-mode 1))

;; Counsel
(use-package counsel
  :config
  (counsel-mode 1))

;; Sort and filter candidates
(use-package prescient)
(use-package ivy-prescient
  :after (prescient counsel ivy)
  :config
  (ivy-prescient-mode))
(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode))

;; Display current match and show count when searching
(use-package anzu
  :config
  (global-anzu-mode t)
  (anzu-mode t))
(use-package evil-anzu
  :after (evil anzu))

;; Easy keybindings
(use-package general
  :config
  (general-evil-setup))

;; Show available keybindings after a delay
(use-package which-key
  :config
  (which-key-mode))

;; Zero-config jump-to-definition
(use-package dumb-jump)

;; Show Git changes in gutter
(use-package git-gutter
  :config
  (custom-set-variables '(git-gutter:modified-sign "~"))
  (global-git-gutter-mode t))

;; Stop the Emacs and system clipboards from mixing
(use-package simpleclip
  :config
  (simpleclip-mode t))

;; Clean up whitespace on lines I've modified
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :config
  (global-hl-todo-mode t))

;; Languages
(use-package haskell-mode
  :defer)
(use-package nix-mode
  :defer)
(use-package purescript-mode
  :defer
  :hook
  (purescript-mode . turn-on-purescript-indentation))
(use-package dhall-mode
  :defer)
(use-package protobuf-mode
  :defer)
(use-package dockerfile-mode
  :defer)
(use-package web-mode
  :defer)
(use-package rust-mode
  :defer)
(use-package lua-mode
  :defer)
(use-package fish-mode
  :defer)
(use-package markdown-mode
  :defer)
(use-package yaml-mode
  :defer
  :hook
  (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Keybindings
(general-mmap
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  ;; "gi" 'evil-first-non-blank
  "gh" 'evil-beginning-of-line
  "gj" 'evil-goto-line
  "gk" 'evil-goto-first-line
  "gl" 'evil-end-of-line)

;; Adjust text scale
(defun text-scale-reset ()
  "Reset the text scale"
  (interactive)
  (text-scale-adjust 0))
(bind-key "s-="	'text-scale-increase)
(bind-key "s--"	'text-scale-decrease)
(bind-key "s-0"	'text-scale-reset)

;; Disable garbage collection during startup
(setq gc-cons-threshold 128000000)

;; Double the default garbage collection threshold after startup
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 1600000)))

;; Announce when garbage collection happens
(setq garbage-collection-messages t)

;; Disable superfluous chrome
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; Disable startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

;; Disable customizations
(setq custom-file "/dev/null")

;; Disable clutter files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Shorten yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(prefer-coding-system 'utf-8)

;; Follow symlinks when opening files under version control
(setq vc-follow-symlinks t)

;; Auto-reload file when modified externally
(global-auto-revert-mode +1)

;; Make files with a shebang executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Sentences shouldn't have to end with two spaces
(setq sentence-end-double-space nil)

;; Indent with 2 spaces
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; Scroll line-by-line
(setq scroll-conservatively 10000)

;; Enable line numbers
(global-display-line-numbers-mode +1)

;; Highlight matching brackets
(show-paren-mode +1)
(setq show-paren-delay 0)

;; Enable mouse support in the terminal
(unless (display-graphic-p)
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

;; Other GUI-specific stuff
(when (display-graphic-p)
  ;; Scroll 3 lines in the GUI, like in the terminal
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)

  ;; Font
  (setq default-frame-alist '((font . "PragmataPro Liga-16")))

  ;; Get rid of weird fonts in org-mode
  (set-face-font 'default "PragmataPro Liga-16")
  (copy-face 'default 'fixed-pitch)
  (copy-face 'default 'variable-pitch)

  ;; Smaller font size adjustment increments
  (setq text-scale-mode-step 1.1))

;; Maximize GUI frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Install straight.el
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

;; Shallow clone packages
(setq straight-vc-git-default-clone-depth 1)

;; Integrate straight.el and use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)

;; Avoid garbage collection until idle
(use-package gcmh
  :config (gcmh-mode +1))

;; Keybindings
(use-package general)

;; Theme
(use-package modus-operandi-theme)
(load-theme 'modus-operandi t)

;; Modeline
(use-package mood-line
  :config (mood-line-mode +1))

;; Highlight TODO, NOTE, FIXME, etc.
(use-package hl-todo
  :config (global-hl-todo-mode +1))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil))
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-move-beyond-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-echo-state nil)
  (general-def
    :states '(normal visual motion)
    ;; Don't skip over wrapped lines
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    ;; Kakoune keys for convenience
    "gi" 'evil-first-non-blank
    "ge" '(lambda () (interactive) (evil-goto-line) (evil-end-of-line))
    "gh" 'evil-beginning-of-line
    "gj" 'evil-goto-line
    "gk" 'evil-goto-first-line
    "gl" 'evil-last-non-blank)
  :config (evil-mode +1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Change cursor shape in terminal based on mode
(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :after evil
  :init
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor 'hbar)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode +1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-escape
  :commands (evil-escape)
  :after evil)

(use-package smartparens
  :init (setq sp-highlight-pair-overlay nil)
  :config (smartparens-global-mode +1))
(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))

;; LSP
(use-package lsp-mode
  :hook
  ((rust-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; Zero-config jump-to-definition
(use-package dumb-jump
  :after ivy
  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (dumb-jump-mode +1))

;; Auto-complete
(use-package company
  :init (setq company-idle-delay 0.2)
  :hook (prog-mode . company-mode))

;; Display errors
(use-package flycheck
  :init
  (setq flycheck-disabled-checkers
	'(emacs-lisp
	  emacs-lisp-checkdoc
	  haskell-ghc
	  haskell-stack-ghc
	  racket))
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config (global-flycheck-mode +1))

(use-package magit
  :init (setq git-commit-summary-max-length 50))
(use-package magit-delta
  :after magit
  :config (magit-delta-mode +1))
(use-package evil-magit
  :after (evil magit))

;; Projectile
(use-package projectile
  :after ivy
  :init (setq projectile-completion-system 'ivy)
  :config (projectile-mode +1))

(use-package org
  :commands (org-mode))
(use-package evil-org
  :after (evil org)
  :init
  (setq org-adapt-indentation nil)
  (setq org-src-tab-acts-natively t)
  :config
  (general-def
    :keymaps 'evil-org-mode-map
    :states 'normal
    "TAB" 'org-cycle)
  (evil-org-set-key-theme)
  :hook
  ((org-mode . evil-org-mode)
   (evil-org-mode . (lambda () (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))))

;; Show search result count in modeline
(use-package anzu
  :config (global-anzu-mode +1))
(use-package evil-anzu
  :after (evil anzu))

(use-package ivy
  :config (ivy-mode +1))

(use-package counsel
  :config (counsel-mode +1))
(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

(use-package prescient
  :config (prescient-persist-mode +1))
(use-package ivy-prescient
  :after (ivy counsel prescient)
  :config (ivy-prescient-mode +1))

;; ;; Terminals in GUI
;; (use-package vterm
;;   :when (display-graphic-p)
;;   :init
;;   (setq vterm-shell "fish")
;;   (setq vterm-max-scrollback 10000))
;; (use-package multi-vterm
;;   :when (display-graphic-p)
;;   :after vterm)

;; Show Git changes in gutter
(use-package git-gutter
  :config
  (custom-set-variables '(git-gutter:modified-sign "~"))
  (global-git-gutter-mode +1))

;; Stop the Emacs and system clipboards from mixing
(use-package simpleclip
  :when (display-graphic-p)
  :config (simpleclip-mode +1))

;; Clean up whitespace on lines I've modified
(use-package ws-butler
  :config (ws-butler-global-mode +1))

;; Get PATH variable from shell
(use-package exec-path-from-shell
  :when (display-graphic-p)
  :after no-littering
  :config (exec-path-from-shell-initialize))

(use-package haskell-mode
  :commands (haskell-mode)
  :hook
  (haskell-mode . (lambda ()
		    (setq-local paragraph-separate "[ \t\f]*$")
		    (setq-local paragraph-start "\f\\|[ \t]*$"))))

(use-package nix-mode
  :commands (nix-mode))

(use-package rustic
  :commands (rustic-mode)
  :init (setq rustic-format-on-save t))

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

;; Show available keybindings
(use-package which-key
  :init (setq which-key-idle-delay 0.5)
  :config (which-key-mode +1))

;; Adjust text scale in GUI
(when (display-graphic-p)
  (defun text-scale-reset ()
    "Reset the text scale"
    (interactive)
    (text-scale-adjust 0))
  (general-def
    "s-=" 'text-scale-increase
    "s--" 'text-scale-decrease
    "s-0" 'text-scale-reset))

;; Leader
(general-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal visual insert emacs)
  :keymaps 'override
  "" '(nil :which-key "leader")
  "ESC" '(evil-escape :which-key t)
  "C-g" '(evil-escape :which-key t)
  "SPC" '(execute-extended-command :which-key "M-x")

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
  "b c" '(evil-buffer-new :which-key "create")
  "b l" '(ibuffer :which-key "list")
  "b s" '(counsel-ibuffer :which-key "switch")
  "b n" '(next-buffer :which-key "next")
  "b p" '(previous-buffer :which-key "previous")
  "b d" '(evil-delete-buffer :which-key "delete")

  "f" '(:ignore t :which-key "file")
  "f C-g" '(evil-escape :which-key t)
  "f ESC" '(evil-escape :which-key t)
  "f s" '(save-buffer :which-key "save")
  "f f" '(find-file :which-key "find")

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
  "p s" '(projectile-switch-project :which-key "switch")
  "p f" '(projectile-find-file :which-key "find file")
  "p d" '(projectile-dired :which-key "dired")
  "p D" '(projectile-dired-other-window :which-key "dired split")
  "p t" '(multi-vterm-projectile :which-key "terminal")

  "p b" '(:ignore t :which-key "buffer")
  "p b C-g" '(evil-escape :which-key t)
  "p b ESC" '(evil-escape :which-key t)
  "p b l" '(projectile-ibuffer :which-key "list")
  "p b s" '(projectile-switch-buffer :which-key "switch")
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
  "h M" '(which-key-show-major-mode :which-key "which-key major mode")
  "h p" '(describe-package :which-key "package")

  "q" '(:ignore t :which-key "help")
  "q C-g" '(evil-escape :which-key t)
  "q q" '(evil-quit :which-key "quit")
  "q k" '(save-buffers-kill-emacs :which-key "kill")

  "," '((lambda () (interactive) (evil-edit "~/.config/emacs/init.el")) :which-key "edit config"))

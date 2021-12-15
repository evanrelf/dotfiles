;;; -*- lexical-binding: t -*-

(use-package use-package
  :defer 0
  :init
  (setq-default use-package-always-ensure t)
  :config
  (eval-and-compile
    (setq-default package-archives '(("melpa" . "https://melpa.org/packages/")
                                     ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)))

(use-package modus-themes
  :config
  (setq-default modus-theme-syntax '(alt-syntax))
  (setq-default modus-themes-mode-line '(accented borderless))
  (setq-default modus-themes-region '(bg-only))
  (setq-default modus-themes-lang-checkers '(background text-also))
  (cond
   ((eq system-type 'gnu/linux)
    (load-theme 'modus-vivendi t))
   ((eq system-type 'darwin)
    (let ((style (shell-command-to-string "begin; defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light; end | tr -d '\n'")))
      (cond
       ((string= style "Light")
        (load-theme 'modus-operandi t))
       ((string= style "Dark")
        (load-theme 'modus-vivendi t)))))))

(use-package ns-auto-titlebar
  :when (and (display-graphic-p) (eq system-type 'darwin))
  :after modus-themes
  :config (ns-auto-titlebar-mode +1))

(use-package mood-line
  :config (mood-line-mode +1))

(use-package no-littering)

(use-package gcmh
  :defer 0
  :config (gcmh-mode +1))

(use-package exec-path-from-shell
  :when (display-graphic-p)
  :after no-littering
  :config (exec-path-from-shell-initialize))

(use-package simpleclip
  :config (simpleclip-mode +1))

(use-package evil
  :defer 0
  :after general
  :init
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-u-scroll t)
  (setq-default evil-want-Y-yank-to-eol t)
  ;; Fix TAB in org-mode when running Emacs in a terminal
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-echo-state nil)
  (setq-default evil-undo-system 'undo-fu)
  (setq-default evil-shift-width 2)
  (setq-default evil-split-window-below t)
  (setq-default evil-vsplit-window-right t)
  (setq-default evil-move-beyond-eol t)
  (general-define-key
   :states '(normal visual motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  :config (evil-mode +1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :after evil
  :init
  (setq-default evil-motion-state-cursor 'box)
  (setq-default evil-visual-state-cursor 'box)
  (setq-default evil-normal-state-cursor 'box)
  (setq-default evil-insert-state-cursor 'bar)
  (setq-default evil-emacs-state-cursor 'hbar)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode +1))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package undo-fu
  :after evil)

(use-package general
  :defer 0)

(use-package which-key
  :defer 0
  :init
  (setq-default which-key-idle-delay 0.5)
  (setq-default which-key-idle-secondary-delay 0.05)
  :config (which-key-mode +1))

(use-package vertico
  :defer 0
  :config
  (vertico-mode +1)
  (vertico-mouse-mode +1))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :defer 0
  :config
  (setq-default consult-find-args "find . -not ( -name .git -prune )")
  (setq-default consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden --glob !.git ."))

(use-package marginalia
  :defer 0
  :config (marginalia-mode +1))

(use-package orderless
  :defer 0
  :config (setq-default completion-styles '(orderless)))

(use-package savehist
  :ensure nil
  :defer 0
  :config (savehist-mode +1))

(use-package autorevert
  :ensure nil
  :defer 0
  :config (global-auto-revert-mode +1))

(use-package winner
  :ensure nil
  :defer 0
  :bind (:map evil-window-map
              ("u" . winner-undo)
              ("U" . winner-redo))
  :config (winner-mode +1))

(use-package projectile
  :defer 0
  :init
  (when (file-directory-p "~/Code/evanrelf")
    (setq projectile-project-search-path '("~/Code/evanrelf")))
  :config (projectile-mode +1))

(use-package consult-projectile
  :after (consult projectile)
  :commands consult-projectile)

(use-package bufler
  :commands bufler
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET") 'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "S" 'bufler-list-buffer-save
    "D" 'bufler-list-buffer-kill))

(use-package magit
  :defer 2
  :commands magit-status
  :config
  (setq-default magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq-default git-commit-summary-max-length 50)
  (setq-default git-commit-style-convention-checks '(overlong-summary-line
                                                     non-empty-second-line))
  :hook (git-commit-mode . (lambda () (setq-local fill-column 72))))

(use-package libgit
  :after magit)

(use-package magit-libgit
  :after (magit libgit))

(use-package display-line-numbers
  :ensure nil
  :init (setq-default display-line-numbers-width 3)
  :config (global-display-line-numbers-mode +1))

(use-package git-gutter
  :defer 0
  :init
  (setq-default git-gutter:added-sign " ")
  (setq-default git-gutter:modified-sign " ")
  (setq-default git-gutter:deleted-sign " ")
  :config (global-git-gutter-mode +1))

(use-package paren
  :ensure nil
  :defer 0
  :init (setq-default show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package smartparens
  :defer 0
  :init (setq-default sp-highlight-pair-overlay nil)
  :config (smartparens-global-mode +1))

(use-package whitespace
  :ensure nil
  :defer 0
  :init
  (setq-default whitespace-style '(face
                                   trailing
                                   tabs
                                   missing-newline-at-eof
                                   empty
                                   tab-mark))
  :config (whitespace-mode +1))

(use-package ws-butler
  :defer 0
  :config (ws-butler-global-mode +1))

(use-package apheleia
  :ensure nil
  :init
  (setq-default apheleia-formatters '((fourmolu "fourmolu" "-o" "-XBangPatterns" "-o" "-XTypeApplications")
                                      (nixpkgs-fmt "nixpkgs-fmt")
                                      (rustfmt "rustfmt")))
  (setq-default apheleia-mode-alist '((haskell-mode . fourmolu)
                                      (nix-mode . nixpkgs-fmt)
                                      (rust-mode . rustfmt)))
  :commands apheleia-format-buffer
  :hook (rust-mode . apheleia-mode))

(use-package flycheck
  :init
  (setq-default flycheck-checkers '(nix
                                    sh-shellcheck))
  (setq-default flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-highlighting-mode 'lines)
  (setq-default flycheck-highlighting-style 'level-face)
  (setq-default flycheck-indication-mode nil)
  (setq-default flycheck-display-errors-delay 0.2)
  :hook ((nix-mode . flycheck-mode)
         (sh-mode . flycheck-mode)))

(use-package flycheck-popup-tip
  :after flycheck
  :init (setq-default flycheck-popup-tip-error-prefix "• ")
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package consult-flycheck
  :after (consult flycheck)
  :commands consult-flycheck)

(use-package hl-todo
  :defer 0
  :config (global-hl-todo-mode +1))

(use-package paren-face
  :hook (emacs-lisp-mode . paren-face-mode))

(use-package org
  :mode ("\\.org\\'" . org-mode))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package haskell-mode
  :mode "\\.hs\\'" "\\.hs-boot\\'" ("\\.cabal\\'" . haskell-cabal-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config (setq-default nix-nixfmt-bin "nixpkgs-fmt"))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package zig-mode
  :mode "\\.zig\\'")

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package purescript-mode
  :mode "\\.purs\\'"
  :hook (purescript-mode . turn-on-purescript-indentation))

(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package web-mode
  :mode "\\.html\\'" "\\.css\\'" "\\.js\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'" "\\.yml\\'")

;; Disable user interface elements
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1))

;; Disable startup messages
(setq-default inhibit-startup-screen t)
(setq-default initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

;; Disable {beginning,end} of buffer messages
;; (https://emacs.stackexchange.com/a/20039)
(defun quieter-command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))
(setq-default command-error-function #'quieter-command-error-function)

;; Disable audio bell
(setq-default ring-bell-function 'ignore)

;; Disable clutter files
(setq-default custom-file null-device)
(setq-default make-backup-files nil)
(setq-default create-lockfiles nil)
(setq-default auto-save-default nil)

;; Shorten yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks when opening files under version control
(setq-default vc-follow-symlinks t)

;; Silence native compilation warnings
(setq-default comp-async-report-warnings-errors nil)

;; Indent with 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default sh-basic-offset 2) ;; sh-mode

;; Add newline at the end of files
(setq-default require-final-newline t)

;; Show ruler at 80 columns
(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

;; Stop wrapping lines at window edge
(setq-default truncate-lines t)

;; Disable documentation on hover
(global-eldoc-mode -1)

;; Widen fringe in GUI frames
(setq-default left-fringe-width 10)

;; Focus and resize GUI frames
(when (display-graphic-p)
  (x-focus-frame nil)
  (setq-default initial-frame-alist '((fullscreen . maximized)))
  (setq-default default-frame-alist
                '((fullscreen . fullheight) (width . 100) (left . 0.5))))

;; Use preferred font in GUI
(when (display-graphic-p)
  ;; Fallback to Iosevka if PragmataPro isn't available
  (cond
   ((find-font (font-spec :name "PragmataPro Liga"))
    (set-face-font 'default "PragmataPro Liga-16"))
   ((find-font (font-spec :name "PragmataPro"))
    (set-face-font 'default "PragmataPro-16"))
   ((find-font (font-spec :name "Iosevka Term SS08"))
    (set-face-font 'default "Iosevka Term SS08-16"))
   ((find-font (font-spec :name "Iosevka Term"))
    (set-face-font 'default "Iosevka Term-16")))

  ;; Use monospaced font for everything
  (copy-face 'default 'fixed-pitch)
  (copy-face 'default 'variable-pitch)

  ;; Adjust font size in smaller increments
  (setq-default text-scale-mode-step 1.1))

;; Make the mouse wheel scroll 3 lines at a time
(setq-default scroll-conservatively 10000)
(when (display-graphic-p)
  (setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq-default mouse-wheel-progressive-speed nil))
(unless (display-graphic-p)
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

;; Confirm when quitting GUI Emacs
(when (display-graphic-p)
  (setq-default confirm-kill-emacs 'yes-or-no-p))

(general-define-key
 :keymaps 'override
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"

 "" '(nil :which-key "leader")
 "C-g" '(keyboard-quit :which-key t)
 "<escape>" '(keyboard-quit :which-key t)
 "SPC" '(execute-extended-command :which-key "command")
 "/" '(consult-ripgrep :which-key "ripgrep")
 "," '((lambda ()
         (interactive)
         (evil-edit "~/.config/emacs/init.el"))
       :which-key "edit config")

 "b" '(:ignore t :which-key "buffer")
 "b C-g" '(keyboard-quit :which-key t)
 "b <escape>" '(keyboard-quit :which-key t)
 "b s" '(consult-buffer :which-key "switch")
 "b l" '(bufler :which-key "list")
 "b `" '(evil-buffer :which-key "last")
 "b n" '(evil-next-buffer :which-key "next")
 "b p" '(evil-prev-buffer :which-key "previous")
 "b d" '(evil-delete-buffer :which-key "delete")
 "b =" '(apheleia-format-buffer :which-key "format")

 "c" '(:ignore t :which-key "clipboard")
 "c C-g" '(keyboard-quit :which-key t)
 "c <escape>" '(keyboard-quit :which-key t)
 "c d" '(simpleclip-cut :which-key "cut")
 "c y" '(simpleclip-copy :which-key "copy")
 "c p" '(simpleclip-paste :which-key "paste")

 "f" '(:ignore t :which-key "file")
 "f C-g" '(keyboard-quit :which-key t)
 "f <escape>" '(keyboard-quit :which-key t)
 "f f" '(consult-find :which-key "find")
 "f s" '(save-buffer :which-key "save")

 "g" '(:ignore t :which-key "git")
 "g C-g" '(keyboard-quit :which-key t)
 "g <escape>" '(keyboard-quit :which-key t)
 "g s" '(magit-status :which-key "status")

 "h" '(:ignore t :which-key "help")
 "h C-g" '(keyboard-quit :which-key t)
 "h <escape>" '(keyboard-quit :which-key t)
 "h a" '(consult-apropos :which-key "apropos")
 "h f" '(describe-function :which-key "function")
 "h k" '(describe-key :which-key "key")
 "h v" '(describe-variable :which-key "variable")

 "p" '(:ignore t :which-key "project")
 "p C-g" '(keyboard-quit :which-key t)
 "p <escape>" '(keyboard-quit :which-key t)
 "p p" '(consult-projectile :which-key "switch")
 "p s" '(projectile-switch-project :which-key "switch project")

 "q" '(:ignore t :which-key "quit")
 "q C-g" '(keyboard-quit :which-key t)
 "q <escape>" '(keyboard-quit :which-key t)
 "q q" '(evil-quit-all :which-key "quit")
 "q k" '(save-buffers-kill-emacs :which-key "quit daemon")
 "q K" '(kill-emacs :which-key "kill daemon")

 "w" '(:ignore t :which-key "window")
 "w C-g" '(keyboard-quit :which-key t)
 "w <escape>" '(keyboard-quit :which-key t)
 "w `" '(evil-window-mru :which-key "focus last")
 "w h" '(evil-window-left :which-key "focus left")
 "w j" '(evil-window-down :which-key "focus down")
 "w k" '(evil-window-up :which-key "focus up")
 "w l" '(evil-window-right :which-key "focus right")
 "w n" '(evil-window-next :which-key "focus next")
 "w p" '(evil-window-prev :which-key "focus previous")
 "w H" '(evil-window-move-far-left :which-key "move left")
 "w J" '(evil-window-move-very-bottom :which-key "move down")
 "w K" '(evil-window-move-very-top :which-key "move up")
 "w L" '(evil-window-move-far-right :which-key "move right")
 "w -" '(evil-window-new :which-key "split horizontally")
 "w \\" '(evil-window-vnew :which-key "split vertically")
 "w d" '(evil-window-delete :which-key "delete")
 "w u" '(winner-undo :which-key "undo")
 "w C-r" '(winner-undo :which-key "redo")
 "w U" '(winner-undo :which-key "redo"))

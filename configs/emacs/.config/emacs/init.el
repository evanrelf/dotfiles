;;; -*- lexical-binding: t -*-

(use-package use-package
  :config
  (setq-default use-package-always-ensure t)
  (eval-and-compile
    (setq-default package-archives '(("org" . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)))

(use-package no-littering)

(use-package gcmh
  :config (gcmh-mode +1))

(use-package exec-path-from-shell
  :when (display-graphic-p)
  :after no-littering
  :config (exec-path-from-shell-initialize))

(use-package evil
  :init
  (setq-default evil-want-keybinding nil)
  (setq-default evil-want-C-u-scroll t)
  (setq-default evil-want-Y-yank-to-eol t)
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

(use-package general)

(use-package which-key
  :init
  (setq-default which-key-idle-delay 0.5)
  (setq-default which-key-idle-secondary-delay 0.05)
  :config (which-key-mode +1))

(use-package vertico
  :config (vertico-mode +1))

(use-package consult)

(use-package marginalia
  :config (marginalia-mode +1))

(use-package orderless
  :config (setq-default completion-styles '(orderless)))

(use-package savehist
  :ensure nil
  :config (savehist-mode +1))

(use-package modus-themes
  :init
  (setq-default modus-theme-syntax '(alt-syntax))
  (setq-default modus-themes-mode-line '(accented borderless))
  (setq-default modus-themes-region '(bg-only))
  :config
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
  :when (eq system-type 'darwin)
  :after modus-themes
  :config (ns-auto-titlebar-mode +1))

(use-package mood-line
  :config (mood-line-mode +1))

(use-package magit
  :commands magit-status
  :config
  (setq-default git-commit-summary-max-length 50)
  (setq-default git-commit-style-convention-checks '(overlong-summary-line
                                                     non-empty-second-line))
  :hook (git-commit-mode-hook . (lambda () (setq-local fill-column 72))))

(use-package libgit
  :after magit)

(use-package magit-libgit
  :after (magit libgit))

(use-package git-gutter
  :init (setq-default git-gutter:modified-sign "~")
  :config (global-git-gutter-mode +1))

(use-package smartparens
  :init (setq-default sp-highlight-pair-overlay nil)
  :config (smartparens-global-mode +1))

(use-package haskell-mode
  :mode "\\.hs\\'" "\\.hs-boot\\'" "\\.cabal\\'")

(use-package nix-mode
  :mode "\\.nix\\'"
  :config (setq-default nix-nixfmt-bin "nixpkgs-fmt"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq-default rust-format-on-save t))

(use-package zig-mode
  :mode "\\.zig\\'")

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

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

(use-package ws-butler
  :config (ws-butler-global-mode +1))

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

;; Indent with 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Add newline at the end of files
(setq-default require-final-newline t)

;; Enable line numbers
(global-display-line-numbers-mode +1)
(setq-default display-line-numbers-width 3)

;; Show ruler at 80 columns
(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

;; Stop wrapping lines at window edge
(setq-default truncate-lines t)

;; Remove delay showing matching parenthesis
(show-paren-mode -1)
(setq-default show-paren-delay 0)
(show-paren-mode +1)

;; Disable documentation on hover
(global-eldoc-mode -1)

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

(general-define-key
 :prefix "SPC"
 :keymaps 'override
 :states '(normal visual)

 "" '(nil :which-key "leader")
 "C-g" '(keyboard-quit :which-key t)
 "<escape>" '(keyboard-quit :which-key t)
 "SPC" '(execute-extended-command :which-key "command")
 "," '((lambda ()
         (interactive)
         (evil-edit "~/.config/emacs/init.el"))
       :which-key "edit config")

 "f" '(:ignore t :which-key "file")
 "f C-g" '(keyboard-quit :which-key t)
 "f <escape>" '(keyboard-quit :which-key t)
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

 "w" '(:ignore t :which-key "window")
 "w C-g" '(keyboard-quit :which-key t)
 "w <escape>" '(keyboard-quit :which-key t)
 "w `" '(evil-window-mru :which-key "split horizontally")
 "w -" '(evil-window-new :which-key "split horizontally")
 "w \\" '(evil-window-vnew :which-key "split vertically")
 "w h" '(evil-window-left :which-key "left")
 "w j" '(evil-window-down :which-key "down")
 "w k" '(evil-window-up :which-key "up")
 "w l" '(evil-window-right :which-key "right")
 "w n" '(evil-window-next :which-key "next")
 "w p" '(evil-window-prev :which-key "previous")
 "w d" '(evil-window-delete :which-key "delete"))

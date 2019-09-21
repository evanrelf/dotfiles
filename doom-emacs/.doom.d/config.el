;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Set font
(setq default-frame-alist '((font . "PragmataPro Liga-16")))
;; Disable menu bar
(menu-bar-mode -1)
;; Disable tool bar
(tool-bar-mode -1)
;; Maximize new GUI frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Disable Flycheck for Emacs Lisp files
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
;; Enable mouse scrolling in the terminal
(unless window-system
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))
;; Change cursor depending on mode
(after! evil-terminal-cursor-changer
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar)
  (unless
    (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))

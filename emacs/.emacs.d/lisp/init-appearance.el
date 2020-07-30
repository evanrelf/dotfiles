(provide 'init-appearance)

;; Disable superfluous chrome
(menu-bar-mode -1)

;; Theme
(use-package modus-operandi-theme)
(load-theme 'modus-operandi t)

;; Modeline
(use-package mood-line
  :config (mood-line-mode +1))

;; Display line numbers
(global-display-line-numbers-mode +1)

;; Always apply syntax highlighting
(global-font-lock-mode +1)

;; Highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-delay 0)

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :config (global-hl-todo-mode t))

;; Disable startup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "evanrelf")

;; Use y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Control mini window sizing
(setq resize-mini-windows t)
(setq max-mini-window-height 10)
(setq minibuffer-scroll-window t)

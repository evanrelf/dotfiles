;; Init
(setq user-init-file load-file-name)
(setq user-emacs-directory (file-name-directory user-init-file))

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
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Install theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(use-package doom-modeline
  :config
  (doom-modeline-init))

;; Install evil
(use-package evil
  :config
  (evil-mode 1))
(use-package evil-escape
  :config
  (evil-escape-mode))
(use-package evil-magit)
(use-package evil-terminal-cursor-changer
  :unless window-system
  :init
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar)
  :config
  (evil-terminal-cursor-changer-activate))

;; Install magit
(use-package magit)
(use-package magit-todos)

;; Configure
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Start
(kill-buffer "*scratch*")
(magit-status)
(delete-other-windows)

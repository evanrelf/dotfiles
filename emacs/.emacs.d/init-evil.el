(provide 'init-evil)

(general-def
  :states '(normal visual)
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-shift-width 2)
  (setq evil-move-beyond-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-echo-state nil)
  :config (evil-mode t))

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
  :after evil)

(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :init (setq evil-want-C-i-jump nil)
  :config (evil-org-set-key-theme))

(use-package evil-magit
  :after (evil magit))

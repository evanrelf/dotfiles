(provide 'init-everything)

;; Increase garbage collection threshold from default 8 MB
(setq gc-cons-threshold 32000000) ;; 32 MB

;; Enable messages to detect thrashing
(setq garbage-collection-messages t)

;; Increase recursion limit
(setq max-lisp-eval-depth 2000)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Disable customizations
(setq custom-file "/dev/null")

;; UTF-8
(prefer-coding-system 'utf-8)

;; Disable clutter files
(setq make-backup-files nil)
(setq auto-save-default nil)

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

;; Scroll line-by-line
(setq scroll-conservatively 10000)

;; Garbage collector magic hack
(use-package gcmh
  :config (gcmh-mode t))

;; Prevent plugins from polluting ~/.emacs.d/ or my $HOME
(use-package no-littering)

;; LSP
(use-package lsp-mode
  :hook
  ((rust-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; Auto-complete
(use-package company
  :init (setq company-idle-delay 0.2)
  :hook (after-init . global-company-mode))

;; Automatically insert and manage closing pairs
(use-package smartparens
  :config (smartparens-global-mode t))

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

;; Amazing Git porcelain
(use-package magit
  :defer 5
  :config
  (setq git-commit-summary-max-length 50))
(use-package magit-delta
  :config (magit-delta-mode))

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

;; Show available keybindings after a delay
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
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

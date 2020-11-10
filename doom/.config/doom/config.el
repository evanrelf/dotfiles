;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(unless EMACS27+ (error "Please use Emacs 27 or newer"))

(use-package! mood-line
  :config (mood-line-mode +1))

(setq +default-want-RET-continue-comments nil)

(after! evil
  (setq evil-echo-state nil)
  (setq evil-move-beyond-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq +evil-want-o/O-to-continue-comments nil))

(after! evil-escape
  (evil-escape-mode -1))

(map!
 :after evil
 :nv "j" 'evil-next-visual-line
 :nv "k" 'evil-previous-visual-line
 (:map evil-window-map
       "-" #'+evil-window-split-a
       "\\" #'+evil-window-vsplit-a))

;; Use regular, 3-levels of cycling, instead of Doom's default of 2-levels. This
;; matches the behavior in `markdown-mode'.
(after! (org evil-org)
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(use-package! flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(after! projectile
  (append projectile-ignored-projects '("~/.config/emacs")))

(use-package! lsp-mode
  :hook
  ((rust-mode . lsp-deferred)
   ;;(haskell-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config (setq lsp-enable-snippet nil))

;; (use-package! lsp-haskell
;;   :config (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(after! haskell
  (setq haskell-process-type 'cabal-new-repl))

(use-package! apheleia
  :init
  (setq apheleia-formatters
        '((deno . ("deno" "fmt" "-"))
          (rustfmt . ("rustfmt"))))
  (setq apheleia-mode-alist
        '((typescript-mode . deno)
          (rustic-mode . rustfmt)))
  :config (apheleia-global-mode +1))

(after! magit
  ;; I don't want to wait for massive diffs to load when writing commit messages
  (remove-hook 'server-switch-hook 'magit-commit-diff))

(use-package! magit-delta
  :after magit
  :config (magit-delta-mode +1))

(after! git-gutter
  (custom-set-variables '(git-gutter:modified-sign "~")))

(after! which-key
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05))

;; Scroll 3 lines at a time
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))))
(unless (display-graphic-p)
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

(when (display-graphic-p)
  (defun text-scale-reset ()
    "Reset the text scale"
    (interactive)
    (text-scale-set 0))
  (map! "C-0" 'text-scale-reset)
  (map! "s-0" 'doom/reset-font-size))

(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

(use-package! org
  :init
  (setq org-directory "~/brain")
  :config
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (defun force-org-hide-leading-stars ()
    "See issue for more info: https://github.com/hlissner/doom-emacs/issues/3076"
    (setq org-hide-leading-stars nil)
    (font-lock-mode -1)
    (font-lock-mode +1))
  (add-hook 'org-mode-hook #'force-org-hide-leading-stars))

(use-package! org-roam
  :after org
  :init (setq org-roam-directory "~/brain/notes"))

;; Make Emacs shut up (https://emacs.stackexchange.com/a/20039)
(defun quieter-command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'quieter-command-error-function)

;; Make files with a shebang executable when saving
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(after! rustic
  ;; Fix error about `-Z' flag
  (setq rustic-flycheck-clippy-params "--message-format=json"))

;; Ask for buffer when splitting window
(after! ivy
  (setq +ivy-buffer-preview t)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(+evil-window-split-a
             +evil-window-vsplit-a
             evil-window-split
             evil-window-vsplit)
    (+ivy/projectile-find-file)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Evan Relf"
      user-mail-address "evan@evanrelf.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(if IS-MAC
    (setq doom-font (font-spec :family "PragmataPro Liga" :size 16)
          doom-big-font (font-spec :family "PragmataPro Liga" :size 20)
          doom-variable-pitch-font (font-spec :family "PragmataPro Liga" :size 16))
  (setq doom-font (font-spec :family "Iosevka Pro" :size 27)
        doom-big-font (font-spec :family "Iosevka Pro" :size 40)
        doom-variable-pitch-font (font-spec :family "Iosevka Pro" :size 27)))

(setq modus-operandi-theme-org-blocks 'greyscale)
(setq doom-theme 'modus-operandi)

(setq display-line-numbers-type t)

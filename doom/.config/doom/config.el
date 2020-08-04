;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(unless EMACS27+ (error "Please use Emacs 27 or newer"))

(mood-line-mode +1)

(after! evil
  (setq evil-echo-state nil)
  (setq evil-move-beyond-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq +evil-want-o/O-to-continue-comments nil))

(use-package! evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :after evil
  :init
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor 'hbar)
  :config (evil-terminal-cursor-changer-activate))

;; TODO: Clean this up
(map!
 :after evil
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line
 :v "j" 'evil-next-visual-line
 :v "k" 'evil-previous-visual-line
 :m "j" 'evil-next-visual-line
 :m "k" 'evil-previous-visual-line)

;; Use regular, 3-levels of cycling, instead of Doom's default of 2-levels. This
;; matches the behavior in `markdown-mode'.
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(use-package! flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-disabled-checkers
        '(emacs-lisp
          emacs-lisp-checkdoc)))

(after! magit
  (magit-delta-mode +1))

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

(defun text-scale-reset ()
  "Reset the text scale"
  (interactive)
  (text-scale-set 0))

(map! "C-0" 'text-scale-reset)

(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

;; (use-package! visual-fill-column
;;   :hook (org-indent-mode . visual-fill-column-mode))

(defun force-org-hide-leading-stars ()
  "See issue for more info: https://github.com/hlissner/doom-emacs/issues/3076"
  (setq org-hide-leading-stars nil)
  (font-lock-mode -1)
  (font-lock-mode +1))

(after! org
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (add-hook 'org-mode-hook #'force-org-hide-leading-stars))

(use-package! org-roam
  :init (setq org-roam-directory "~/brain/notes"))

;; Make Emacs shut up (https://emacs.stackexchange.com/a/20039)
(defun quieter-command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'quieter-command-error-function)

;; Make files with a shebang executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

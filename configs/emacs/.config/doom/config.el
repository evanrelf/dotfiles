;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!



(use-package! modus-themes
  :config
  ;; Borderless modeline
  (setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive))))

(use-package! mood-line
  :config (mood-line-mode +1))

(after! paren
  (setq show-paren-delay 0))

(after! which-key
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.05))

(setq user-full-name "Evan Relf"
      user-mail-address "evan@evanrelf.com")

(setq doom-font (font-spec :family "PragmataPro" :size 16))

(setq doom-theme 'modus-operandi)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

;; Scroll 3 lines at a time
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))))
(unless (display-graphic-p)
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

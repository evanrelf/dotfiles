;;; -*- lexical-binding: t -*-

(leaf leaf
  :ensure nil
  :config
  (eval-and-compile
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)))

(leaf exec-path-from-shell
  :ensure t
  :when (display-graphic-p)
  :config (exec-path-from-shell-initialize))

(leaf evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-echo-state nil)
  (setq evil-undo-system 'undo-fu)
  (general-define-key
   :states '(normal visual motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  :config (evil-mode +1))

(leaf evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(leaf evil-commentary
  :ensure t
  :config (evil-commentary-mode +1))

(leaf evil-indent-plus
  :ensure t
  :config (evil-indent-plus-default-bindings))

(leaf undo-fu
  :ensure t)

(leaf general
  :ensure t)

(leaf which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  :config (which-key-mode +1))

(leaf vertico
  :ensure t
  :config (vertico-mode +1))

(leaf consult
  :ensure t)

(leaf marginalia
  :ensure t
  :config (marginalia-mode +1))

(leaf orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

(leaf savehist
  :ensure nil
  :config (savehist-mode +1))

(leaf modus-themes
  :ensure t
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

(leaf ns-auto-titlebar
  :ensure t
  :when (eq system-type 'darwin)
  :config (ns-auto-titlebar-mode +1))

(leaf mood-line
  :ensure t
  :config (mood-line-mode +1))

(leaf magit
  :ensure t
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks '(overlong-summary-line
					     non-empty-second-line))
  (add-hook 'git-commit-mode-hook #'(lambda () (setq fill-column 72))))

(leaf git-gutter
  :ensure t
  :init (setq git-gutter:modified-sign "~")
  :config (global-git-gutter-mode +1))

(leaf smartparens
  :ensure t
  :init (setq sp-highlight-pair-overlay nil)
  :config (smartparens-global-mode +1))

(leaf haskell-mode
  :ensure t)

(leaf nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :config (setq nix-nixfmt-bin "nixpkgs-fmt"))

(leaf rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t))

(leaf zig-mode
  :ensure t)

(leaf vterm
  :ensure nil
  :config
  (add-hook 'vterm-mode-hook #'(lambda ()
				 (display-line-numbers-mode -1)
				 (display-fill-column-indicator-mode -1)
				 (setq mode-line-format nil))))

(leaf multi-vterm
  :ensure t)

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
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(fset 'display-startup-echo-area-message 'ignore)

;; Disable {beginning,end} of buffer messages
;; (https://emacs.stackexchange.com/a/20039)
(defun quieter-command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-buffer
				end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'quieter-command-error-function)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Disable clutter files
(setq custom-file "/dev/null")
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; Shorten yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks when opening files under version control
(setq vc-follow-symlinks t)

;; Indent with 2 spaces
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; Enable line numbers
(global-display-line-numbers-mode +1)
(setq display-line-numbers-width 3)

;; Show ruler at 80 columns
(setq-default fill-column 81)
(global-display-fill-column-indicator-mode +1)

;; Remove delay showing matching parenthesis
(show-paren-mode -1)
(setq show-paren-delay 0)
(show-paren-mode +1)

;; Disable documentation on hover
(global-eldoc-mode -1)

;; Focus and resize GUI frames
(when (display-graphic-p)
  (x-focus-frame nil)
  (setq initial-frame-alist '((fullscreen . maximized)))
  (setq default-frame-alist
	'((fullscreen . fullheight) (width . 100) (left . 0.5))))

;; Use preferred font in GUI
(when (display-graphic-p)
  (cond
   ((find-font (font-spec :name "PragmataPro Liga"))
    (set-frame-font "PragmataPro Liga 16" nil t))
   ((find-font (font-spec :name "PragmataPro"))
    (set-frame-font "PragmataPro 16" nil t))
   ((find-font (font-spec :name "Iosevka Term SS08"))
    (set-frame-font "Iosevka Term SS08 16" nil t))
   ((find-font (font-spec :name "Iosevka Term"))
    (set-frame-font "Iosevka Term 16" nil t))))

;; Make the mouse wheel scroll 3 lines at a time
(setq scroll-conservatively 10000)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))
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
 "h f" '(describe-function :which-key "function")
 "h k" '(describe-key :which-key "key")
 "h v" '(describe-variable :which-key "variable")

 "t" '(:ignore t :which-key "terminal")
 "t C-g" '(keyboard-quit :which-key t)
 "t <escape>" '(keyboard-quit :which-key t)
 "t c" '(multi-vterm :which-key "create")
 "t t" '(multi-vterm-dedicated-toggle :which-key "toggle (dedicated)")
 "t p" '(multi-vterm-project :which-key "toggle (project)")
 "t n" '(multi-vterm-next :which-key "next")
 "t p" '(multi-vterm-prev :which-key "prev")

 "," '((lambda ()
	 (interactive)
	 (evil-edit "~/.config/emacs/init.el"))
       :which-key "edit config"))

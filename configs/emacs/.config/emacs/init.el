;;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq-default straight-use-package-by-default t)

(use-package use-package
  :init (setq-default use-package-always-ensure t))

;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-operandi t))

(use-package ef-themes
  :config (load-theme 'ef-light t))

(use-package evil
  :config (evil-mode +1))

(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1))

(setq-default ring-bell-function 'ignore)

(when (display-graphic-p)
  (cond
   ((find-font (font-spec :name "PragmataPro"))
    (set-face-font 'default "PragmataPro-16"))
   ((find-font (font-spec :name "Iosevka Term SS08"))
    (set-face-font 'default "Iosevka Term SS08-16"))
   ((find-font (font-spec :name "Iosevka Term"))
    (set-face-font 'default "Iosevka Term-16")))
  (copy-face 'default 'fixed-pitch)
  (copy-face 'default 'variable-pitch))

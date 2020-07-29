(provide 'init-gui)

(when (display-graphic-p)
  ;; Font
  (setq default-frame-alist '((font . "PragmataPro Liga-16")))

  ;; Smaller font size adjustment increments
  (setq text-scale-mode-step 1.1)

  ;; Adjust text scale
  (defun text-scale-reset ()
    "Reset the text scale"
    (interactive)
    (text-scale-adjust 0))
  (general-def
    "s-=" 'text-scale-increase
    "s--" 'text-scale-decrease
    "s-0" 'text-scale-reset)

  ;; Maximize GUI frames
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable superfluous chrome
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1)

  ;; Scroll 3 lines in GUI
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)

  ;; Terminals in GUI
  (use-package vterm
    :init
    (setq vterm-shell "fish")
    (setq vterm-max-scrollback 10000))
  (use-package multi-vterm)

  ;; Get PATH variable from shell
  (use-package exec-path-from-shell
    :after no-littering
    :config (exec-path-from-shell-initialize)))

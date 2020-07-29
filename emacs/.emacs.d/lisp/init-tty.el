(provide 'init-tty)

(unless (display-graphic-p)
  ;; Enable mouse support in terminal
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3)))

  ;; Change cursor shape in terminal based on mode
  (use-package evil-terminal-cursor-changer
    :after evil
    :init
    (setq evil-motion-state-cursor 'box)
    (setq evil-visual-state-cursor 'box)
    (setq evil-normal-state-cursor 'box)
    (setq evil-insert-state-cursor 'bar)
    (setq evil-emacs-state-cursor 'hbar)
    :config (evil-terminal-cursor-changer-activate)))

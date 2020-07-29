(provide 'init-languages)

(use-package org
  :commands (org-mode))

(use-package haskell-mode
  :commands (haskell-mode)
  :hook
  (haskell-mode . (lambda ()
		    (setq-local paragraph-separate "[ \t\f]*$")
		    (setq-local paragraph-start "\f\\|[ \t]*$"))))

(use-package nix-mode
  :commands (nix-mode))

(use-package rustic
  :commands (rustic-mode)
  :init (setq rustic-format-on-save t))

(use-package purescript-mode
  :commands (purescript-mode)
  :hook (purescript-mode . turn-on-purescript-indentation))

(use-package dhall-mode
  :commands (dhall-mode))

(use-package racket-mode
  :commands (racket-mode))

(use-package protobuf-mode
  :commands (protobuf-mode))
(use-package dockerfile-mode
  :commands (dockerfile-mode))

(use-package web-mode
  :commands (web-mode))

(use-package lua-mode
  :commands (lua-mode))

(use-package fish-mode
  :commands (fish-mode))

(use-package markdown-mode
  :commands (markdown-mode))

(use-package yaml-mode
  :commands (yaml-mode)
  :hook (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

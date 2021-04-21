;;; init.el -*- lexical-binding: t; -*-

(doom!
  :completion
  company
  (ivy +prescient)

  :ui
  doom-dashboard
  hl-todo
  (popup +defaults)
  vc-gutter

  :editor
  (evil +everywhere)
  fold

  :emacs
  dired
  electric
  undo
  vc

  :term
  vterm

  :checkers
  syntax
  (spell +aspell)

  :tools
  direnv
  lookup
  lsp
  magit
  terraform

  :os
  tty

  :lang
  data
  haskell
  json
  javascript
  markdown
  nix
  org
  (rust +lsp)
  (sh +fish)
  web
  yaml

  :config
  (default +bindings +smartparens))

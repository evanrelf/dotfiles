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
  multiple-cursors

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
  editorconfig
  lookup
  lsp
  magit
  terraform

  :os
  tty

  :lang
  data
  haskell
  idris
  json
  javascript
  markdown
  nix
  org
  racket
  (rust +lsp)
  (sh +fish)
  web
  yaml

  :config
  (default +bindings +smartparens))

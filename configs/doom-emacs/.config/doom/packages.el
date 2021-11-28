;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia"))
(package! git-gutter)
(package! magit-delta)
(package! modus-themes)
(package! mood-line)
(package! solaire-mode)
(package! janet-mode)

(disable-packages!
 ;; core
 hl-line
 saveplace

 ;; :completion ivy
 amx
 wgrep

 ;; :editor evil
 evil-args
 evil-easymotion
 evil-embrace
 evil-exchange
 evil-quick-diff
 evil-snipe
 exato

 ;; :emacs undo
 undo-fu-session

 ;; :lang javascript
 coffee-mode
 js2-refactor
 nodejs-repl
 npm-mode
 rjsx-mode
 skewer-mode
 tide

 ;; :lang json
 json-snatcher

 ;; :lang nix
 company-nixos-options

 ;; :lang org
 htmlize
 org-cliplink
 org-superstar
 org-yt
 orgit
 ox-clip

 ;; :lang rust
 racer

 ;; :lang web
 counsel-css
 haml-mode
 less-css-mode
 pug-mode
 rainbow-mode
 sass-mode
 slim-mode
 stylus-mode
 sws-mode

 ;; :tools lsp
 lsp-ui

 ;; :tools magit
 github-review
 ;; magit-gitflow ;; TODO: I need this enabled for the ? popup

 ;; :ui vc-gutter
 git-gutter-fringe)

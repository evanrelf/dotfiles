;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! apheleia :pin "6aff83d" :recipe (:host github :repo "raxod502/apheleia"))
(package! git-gutter :pin "2c3242")
(package! magit-delta :pin "d988ab")
(package! modus-operandi-theme :pin "c376b0")
(package! mood-line :pin "64cbd6")

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

 ;; :tools lookup
 request

 ;; :tools magit
 github-review
 ;; magit-gitflow ;; TODO: I need this enabled for the ? popup

 ;; :ui doom
 solaire-mode

 ;; :ui vc-gutter
 git-gutter-fringe)

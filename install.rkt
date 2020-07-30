#!/usr/bin/env racket

#lang racket

(define/contract dry-run
  (parameter/c boolean?)
  (make-parameter #f))

(define/contract packages
  (listof string?)
  (command-line
   #:program "install"
   #:once-each
   [("--dry-run") "Run in dry run mode" (dry-run #t)]
   #:args ps
   (map (lambda (s) (string-replace s "/" "")) ps)))

(define/contract (home path)
  (-> string? string?)
  (string-append (path->string (find-system-path 'home-dir)) "/" path))

(define/contract (run command)
  (-> string? any)
  (if (dry-run)
      (printf "dry-run> ~a\n" command)
      (unless (system command) (exit 1))))

(define/contract (check-installed executable)
  (-> string? any)
  (unless (find-executable-path executable)
    (printf "Missing executable: ~a\n" executable)
    (exit 1)))

(define/contract (discard-nonexistent packages)
  (-> (listof string?) (listof string?))
  (define-values (existent nonexistent) (partition directory-exists? packages))
  (for-each
   (curry printf "[~a] Configuration doesn't exist\n")
   nonexistent)
  existent)

(define/contract (prepare-emacs)
  (-> any)
  (printf "[emacs] Setting up truecolor support\n")
  (run "$HOME/dotfiles/emacs/.config/emacs/setup-truecolor"))

(define/contract (prepare-hammerspoon)
  (-> any)
  (printf "[hammerspoon] Changing config file location\n")
  (check-installed "defaults")
  (run
   (string-join
    '("defaults write org.hammerspoon.Hammerspoon MJConfigFile"
      "\"$HOME/.config/hammerspoon/init.lua\""))))

(define/contract (prepare-kakoune)
  (-> any)
  (unless (directory-exists? (home ".config/kak/plugins/plug.kak"))
    (printf "[kakoune] Installing plug.kak\n")
    (check-installed "git")
    (run
     (string-join
      '("git clone --depth=1 'https://github.com/andreyorst/plug.kak.git'"
        "\"$HOME/.config/kak/plugins/plug.kak\"")))))

(define/contract (prepare-neovim)
  (-> any)
  (unless (file-exists? (home ".local/share/nvim/site/autoload/plug.vim"))
    (printf "[neovim] Installing vim-plug\n")
    (check-installed "curl")
    (run
     (string-join
      '("curl --location --fail --create-dirs"
        "'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'"
        "-o \"$HOME/.local/share/nvim/site/autoload/plug.vim\"")))))

(define/contract (prepare-tmux)
  (-> any)
  (unless (directory-exists? (home ".config/tmux/plugins/tpm"))
    (printf "[tmux] Installing tpm\n")
    (check-installed "git")
    (run
     (string-join
      '("git clone --depth=1 'https://github.com/tmux-plugins/tpm.git'"
        "\"$HOME/.config/tmux/plugins/tpm\"")))))

(define/contract (prepare package)
  (-> string? any)
  (case package
    [("emacs") (prepare-emacs)]
    [("hammerspoon") (prepare-hammerspoon)]
    [("kakoune") (prepare-kakoune)]
    [("neovim") (prepare-neovim)]
    [("tmux") (prepare-tmux)]))

(define/contract (stow package)
  (-> string? any)
  (printf "[~a] Stowing configuration\n" package)
  (check-installed "stow")
  (run (format "stow --stow --target=\"$HOME\" --no-folding ~a" package)))

(define/contract (install package)
  (-> string? any)
  (prepare package)
  (stow package))

(define/contract (main)
  (-> any)
  (when (empty? packages)
    (printf "No packages specified\n")
    (exit 1))
  (for-each install (discard-nonexistent packages)))

(main)

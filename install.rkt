#!/usr/bin/env racket

#lang racket

(define dry-run (make-parameter #f))

(define (run command)
  (if (dry-run)
    (printf "dry-run> ~a\n" command)
    (unless (system command) (exit 1))))

(define (home/ path)
  (define home (path->string (find-system-path 'home-dir)))
  (string-append home "/" path))

(define (check-installed executable)
  (unless (find-executable-path executable)
    (printf "Missing executable: ~a\n" executable)
    (exit 1)))

(define (prepare-hammerspoon)
  (printf "[hammerspoon] Changing config file location\n")
  (check-installed "defaults")
  (run
    (format "~a ~s"
      "defaults write org.hammerspoon.Hammerspoon MJConfigFile"
      "$HOME/.config/hammerspoon/init.lua")))

(define (prepare-kakoune)
  (unless (directory-exists? (home/".config/kak/plugins/plug.kak"))
    (printf "[kakoune] Installing plug.kak\n")
    (check-installed "git")
    (run
      (format "~a ~s"
        "git clone --depth=1 https://github.com/andreyorst/plug.kak.git"
        "$HOME/.config/kak/plugins/plug.kak"))))

(define (prepare-neovim)
  (unless (file-exists? (home/".local/share/nvim/site/autoload/plug.vim"))
    (printf "[neovim] Installing vim-plug\n")
    (check-installed "curl")
    (run
      (format "~a ~s ~a"
        "curl --location --fail --create-dirs"
        "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
        "--output=\"$HOME/.local/share/nvim/site/autoload/plug.vim\""))))

(define (prepare-tmux)
  (unless (directory-exists? (home/".config/tmux/plugins/tpm"))
    (printf "[tmux] Installing tpm\n")
    (check-installed "git")
    (run
      (format "~a ~s"
        "git clone --depth 1 'https://github.com/tmux-plugins/tpm.git'"
        "$HOME/.config/tmux/plugins/tpm"))))

(define (prepare package)
  (case package
    [("hammerspoon") (prepare-hammerspoon)]
    [("kakoune") (prepare-kakoune)]
    [("neovim") (prepare-neovim)]
    [("tmux") (prepare-tmux)]))

(define (stow package)
  (printf "[~a] Stowing configuration\n" package)
  (check-installed "stow")
  (run (format "stow --stow --target=$HOME --no-folding ~a" package)))

(define (install package)
  (prepare package)
  (stow package))

(define (discard-nonexistent packages)
  (define-values (existent nonexistent) (partition directory-exists? packages))
  (for-each
    (lambda (x) (printf "[~a] Configuration doesn't exist\n" x))
    nonexistent)
  existent)

(define packages
  (command-line
    #:program "install"
    #:once-each
    [("--dry-run") "Run in dry run mode" (dry-run #t)]
    #:args ps
    (map (lambda (s) (string-replace s "/" "")) ps)))

(define (main)
  (when (empty? packages)
    (printf "No packages specified\n")
    (exit 1))

  (define existent-packages (discard-nonexistent packages))

  (for-each install existent-packages))

(main)

#!/usr/bin/env racket

#lang racket

(define verbose (make-parameter #f))
(define dry-run (make-parameter #f))

(define packages
  (command-line
    #:program "install"
    #:once-each
    [("--verbose") "Print verbose output" (verbose #t)]
    [("--dry-run") "Run in dry run mode" (dry-run #t)]
    #:args ps
    (map (lambda (s) (string-replace s "/" "")) ps)))

(define (pre-install package)
  (when (verbose) (printf "Running pre-install step for ~a" package))
  (case package
    [("hammerspoon") (void)]
    [("kakoune") (void)]
    [("neovim") (void)]
    [("tmux") (void)]))

(define (stow package)
  (when (verbose) (printf "Stowing ~a\n" package))
  (system (format "stow --target=$HOME --no-folding --stow=~a" package)))

(define (install package)
  (if (dry-run)
    (printf "Pretending to install ~a\n" package)
    (printf "This would actually install ~a\n" package)))

(when (empty? packages)
  (printf "No packages specified\n")
  (exit 1))

(for-each install packages)

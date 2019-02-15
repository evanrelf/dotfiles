if _exists git; and status --is-interactive
  abbr --add e "emacsclient -s term -t"
end

alias emacs-daemon "emacs --daemon=term; emacs --daemon=gui"
alias emacs-gui "emacsclient -s gui -c -n"
alias emacs-tui "emacsclient -s term -t"

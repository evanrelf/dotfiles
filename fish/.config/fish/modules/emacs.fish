if _exists git; and status --is-interactive
  abbr --add e "emacs -nw"
  abbr --add ed "emacs --daemon=term; emacs --daemon=gui"
  abbr --add et "emacsclient -s term -t"
  abbr --add eg "emacsclient -s gui -c -n"
end

alias emacs-daemon "emacs --daemon=term; emacs --daemon=gui"
alias emacs-tui "emacsclient -s term -t"
alias emacs-gui "emacsclient -s gui -c -n"

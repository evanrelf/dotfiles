if _exists git; and status --is-interactive
  abbr --add e "emacs -nw"
  # abbr --add ed "emacs --daemon=term; emacs --daemon=gui"
  # abbr --add et "emacsclient -s term -t"
  # abbr --add eg "emacsclient -s gui -c -n"
end

function emacs
  set -l dump_file "$HOME/.emacs.d/.cache/dumps/spacemacs.pdmp"
  if test -e $dump_file
    command emacs --dump-file="$dump_file" $argv
  else
    command emacs $argv
  end
end

# alias emacs-daemon "emacs --daemon=term; emacs --daemon=gui"
# alias emacs-tui "emacsclient -s term -t"
# alias emacs-gui "emacsclient -s gui -c -n"

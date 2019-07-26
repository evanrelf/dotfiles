if _exists git; and status --is-interactive
  abbr --add et "emacs -nw"
end

function e
  set -l emacs_dump_file "$HOME/.emacs.d/.cache/dumps/spacemacs.pdmp"
  command emacs --dump-file="$emacs_dump_file" $argv & disown
end

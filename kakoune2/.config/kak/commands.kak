define-command -docstring "Strip trailing whitespace" \
strip-whitespace -params 0 %{
  execute-keys -draft \%s\h+$<ret>d
}

define-command -docstring "Set filetype" \
filetype -params 1 %{
  set-option window filetype %arg{1}
}

# TODO: Split current buffer if no argument provided
define-command -docstring "Split tmux vertically" \
vsplit -params 0..1 -file-completion %{
  tmux-terminal-horizontal kak -c %val{session} "%arg{@}"
}
alias global vs vsplit

define-command -docstring "Split tmux horizontally" \
split -params 0..1 -file-completion %{
  tmux-terminal-vertical kak -c %val{session} "%arg{@}"
}
alias global sp split

define-command -docstring "Create new tmux window" \
tabnew -params 0..1 -file-completion %{
  tmux-terminal-window kak -c %val{session} "%arg{@}"
}

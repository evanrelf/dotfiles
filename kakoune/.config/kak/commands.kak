define-command -docstring 'Strip trailing whitespace' \
strip-whitespace -params 0 %{
  execute-keys -draft \%s\h+$<ret>d
}

define-command -docstring "Split tmux vertically" \
vsplit -params .. -command-completion %{
  tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}
alias global vs vsplit

define-command -docstring "Split tmux horizontally" \
split -params .. -command-completion %{
  tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}
alias global sp split

define-command -docstring "Create new tmux window" \
tabnew -params .. -command-completion %{
  tmux-terminal-window kak -c %val{session} -e "%arg{@}"
}

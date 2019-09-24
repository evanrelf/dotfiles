define-command -docstring "Strip trailing whitespace" \
strip-whitespace -params 0 %{
  execute-keys -draft \%s\h+$<ret>d
}

define-command -docstring "Set filetype" \
filetype -params 1 %{
  set-option buffer filetype %arg{1}
}
alias global ft filetype

define-command -docstring "Split tmux vertically" \
vsplit -params .. -file-completion %{
  tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}
alias global vs vsplit

define-command -docstring "Split tmux horizontally" \
split -params .. -file-completion %{
  tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}
alias global sp split

define-command -docstring "Create new tmux window" \
tabnew -params .. -file-completion %{
  tmux-terminal-window kak -c %val{session} -e "%arg{@}"
}

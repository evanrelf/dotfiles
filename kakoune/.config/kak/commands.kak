define-command -docstring "strip-whitespace: strip trailing whitespace" \
strip-whitespace %{
  execute-keys -draft \%s\h+$<ret>d
}

# Extend line-based selections
define-command -docstring "extend-line-down: extend selection with line down" \
extend-line-down -params 1 %{
  execute-keys "<a-:>%arg{1}X"
}
define-command -docstring "extend-line-up: extend selection with line up" \
extend-line-up -params 1 %{
  execute-keys "<a-:><a-;>%arg{1}K<a-;>"
  try %{
    execute-keys -draft ";<a-K>\n<ret>"
    execute-keys "X"
  }
  execute-keys "<a-;><a-X>"
}

# Soft wrapping
define-command -docstring "softwrap-enable: enable soft wrapping" \
softwrap-enable %{
  add-highlighter window/softwrap wrap
}

define-command -docstring "softwrap-disable: disable soft wrapping" \
softwrap-disable %{
  remove-highlighter window/softwrap
}

# tmux splits
define-command -docstring "vsplit <filename>: open file in vertical tmux split" \
vsplit -params 0.. -file-completion %{
  # tmux-terminal-horizontal sh -c "kak -c %val{session} %arg{@}; fish"
  tmux-terminal-horizontal kak -c %val{session} %arg{@}
}
alias global vs vsplit

define-command -docstring "split <filename>: open file in horizontal tmux split" \
split -params 0.. -file-completion %{
  # tmux-terminal-vertical sh -c "kak -c %val{session} %arg{@}; fish"
  tmux-terminal-vertical kak -c %val{session} %arg{@}
}
alias global sp split

# Evaluate
define-command -docstring "evaluate-buffer: evaluate buffer commands as if entered by user" \
evaluate-buffer %{
  execute-keys -draft "%: <space><c-r>.<ret>"
}

define-command -docstring "evaluate-selection: evaluate selection commands as if entered by user" \
evaluate-selection %{
  execute-keys -itersel -draft ": <space><c-r>.<ret>"
}

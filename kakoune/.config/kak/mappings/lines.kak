# Allow selecting by line in both directions
map global normal "x" ": extend-line-down %%val{count}<ret>"
map global normal "X" ": extend-line-up %%val{count}<ret>"

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


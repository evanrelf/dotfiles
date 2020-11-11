map global "normal" "x" ": drag-down %%val{count}<ret>"
map global "normal" "X" ": drag-up %%val{count}<ret>"

# TODO: This doesn't work in reverse (grow above, shrink above)

define-command drag-down -params 1 %{
  try %{
    # If on newline, continue expanding
    execute-keys -draft ";<a-k>\n<ret>"
    grow-below "%arg{1}"
  } catch %{
    # Otherwise, we need to make the initial line selection
    execute-keys "Gh<a-:>"
    grow-to-eol
  }
}

# Contract line-based selection upwards
define-command drag-up -params 1 %{
  shrink-below "%arg{1}"
}

# map global "normal" "{" ": grow-above %%val{count}<ret>"
# map global "normal" "[" ": shrink-above %%val{count}<ret>"
# map global "normal" "}" ": grow-below %%val{count}<ret>"
# map global "normal" "]" ": shrink-below %%val{count}<ret>"

define-command -hidden grow-to-eol %{
  execute-keys "Gl"
  try %{
    execute-keys -draft ";<a-K>\n<ret>"
    execute-keys "L"
  }
}

define-command -hidden grow-above -params 1 %{
  execute-keys "<a-:><a-;>Gh%arg{1}K"
}

define-command -hidden shrink-above -params 1 %{
  execute-keys "<a-:><a-;>Gh%arg{1}J"
}

define-command -hidden grow-below -params 1 %{
  execute-keys "<a-:>%arg{1}J"
  grow-to-eol
}

define-command -hidden shrink-below -params 1 %{
  execute-keys "<a-:>%arg{1}K"
  grow-to-eol
}

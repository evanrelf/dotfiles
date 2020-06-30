# Display Git diff in gutter
hook global WinCreate .* %{
  hook -once window NormalIdle .* %{ try %{
    try %{ _number-toggle-refresh }
    git show-diff
    hook window BufWritePost .* %{ try %{
      git update-diff
    }}
  }}
}

# Highlight trailing whitespace
hook global ModeChange push:.*:insert %{
  remove-highlighter window/trailing-whitespace
}
hook global ModeChange pop:insert:.* %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:default,rgb:ff869a
}

# Use Tab key in autocomplete menu
hook global InsertCompletionShow .* %{ try %{
  execute-keys -draft "h<a-K>\h<ret>"
  map window insert "<tab>" "<c-n>"
  map window insert "<s-tab>" "<c-p>"
}}
hook global InsertCompletionHide .* %{
  unmap window insert "<tab>" "<c-n>"
  unmap window insert "<s-tab>" "<c-p>"
}

# Insert and delete spaces for indentation
hook global InsertChar \t %{
  try %{
    execute-keys -draft "hGh<a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@"
  } catch %{
    # execute-keys -draft "<esc>h%opt{indentwidth}@"
    # execute-keys -draft "<esc>hd<gt>"
    execute-keys -draft "<esc>hd"
  }
}
hook global InsertDelete ' ' %{ try %{
  execute-keys -draft 'hGh<a-k>\A\h+\z<ret>i<space><esc><lt>'
}}

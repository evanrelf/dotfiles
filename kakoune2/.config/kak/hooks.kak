# Display Git diff in gutter
hook buffer WinCreate .* %{ git show-diff }
hook buffer BufWritePost .* %{ git update-diff }

# Highlight trailing whitespace
hook window InsertBegin .* %{ remove-highlighter window/trailing-whitespace }
hook window InsertEnd .* %{ add-highlighter window/trailing-whitespace regex \h+$ 0:default,red }

# Use Tab key in autocomplete menu
hook window InsertCompletionShow .* %{
  try %{
    execute-keys -draft "h<a-K>\h<ret>"
    map window insert <tab> <c-n>
    map window insert <s-tab> <c-p>
  }
}
hook window InsertCompletionHide .* %{
  unmap window insert <tab> <c-n>
  unmap window insert <s-tab> <c-p>
}

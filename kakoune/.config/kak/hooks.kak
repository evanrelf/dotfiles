# Display Git diff in gutter
hook global WinCreate .* %{ try %{ git show-diff } }
hook global BufWritePost .* %{ try %{ git update-diff } }

# Highlight trailing whitespace
hook global InsertBegin .* %{ remove-highlighter window/trailing-whitespace }
hook global InsertEnd .* %{ add-highlighter window/trailing-whitespace regex \h+$ 0:default,red }

# Use Tab key in autocomplete menu
hook global InsertCompletionShow .* %{
  try %{
    execute-keys -draft "h<a-K>\h<ret>"
    map window insert <tab> <c-n>
    map window insert <s-tab> <c-p>
  }
}
hook global InsertCompletionHide .* %{
  unmap window insert <tab> <c-n>
  unmap window insert <s-tab> <c-p>
}

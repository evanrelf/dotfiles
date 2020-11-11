# Use spaces for indentation
hook global InsertChar \t %{ try %{
  # Assert cursor is at beginning of line
  execute-keys -draft "hGh<a-k>\A\h+\z<ret>"
  # Replace tab character with spaces
  execute-keys -draft "h%opt{indentwidth}@"
} catch %{
  # Delete tab character when cursor is not at beginning of line
  execute-keys -draft "hd"
}}

# De-indent when deleting a line's leading whitespace
hook global InsertDelete ' ' %{ try %{
  # Assert cursor is at beginning of line
  execute-keys -draft "hGh<a-k>\A\h+\z<ret>"
  # De-indent line
  execute-keys -draft "i<space><esc><lt>"
}}

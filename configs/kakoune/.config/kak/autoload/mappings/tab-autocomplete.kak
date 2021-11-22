# Use Tab in autocomplete menu
hook global InsertCompletionShow .* %{ try %{
  # Assert previous character is not a space
  execute-keys -draft "h<a-K>\h<ret>"
  # Bind tab keys to control completion menu
  map window "insert" "<tab>" "<c-n>"
  map window "insert" "<s-tab>" "<c-p>"
}}
hook global InsertCompletionHide .* %{
  # Bind tab keys back to inserting tab character
  unmap window "insert" "<tab>" "<c-n>"
  unmap window "insert" "<s-tab>" "<c-p>"
}

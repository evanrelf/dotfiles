# Strip trailing whitespace
define-command -docstring "strip-whitespace: strip trailing whitespace" \
strip-whitespace %{
  execute-keys -draft "%%s\h+$<ret>d"
}

# Highlight trailing whitespace
define-command -hidden add-highlighter-trailing-whitespace %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:red+r
}
hook global WinCreate .* %{
  add-highlighter-trailing-whitespace
}
hook global ModeChange pop:insert:.* %{
  try %{ add-highlighter-trailing-whitespace }
}
hook global ModeChange push:.*:insert %{
  remove-highlighter window/trailing-whitespace
}

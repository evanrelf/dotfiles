define-command -docstring "strip: strip trailing whitespace" \
strip %{
  execute-keys -draft "%%s\h+$<ret>d"
}

define-command -hidden add-highlighter-trailing-whitespace %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:red+r
}

hook global WinCreate .* %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:red+r
}

hook global ModeChange pop:insert:.* %{ try %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:red+r
}}

hook global ModeChange push:.*:insert %{
  remove-highlighter window/trailing-whitespace
}

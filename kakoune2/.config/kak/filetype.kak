# Haskell
hook global WinSetOption filetype=haskell %{
  set-option window lintcmd "hlint"
  set-option window formatcmd "sort-imports"
  # Highlight function name in type signatures
  add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]['\w]*)\s+::\s 1:meta
  # Highlight function name in function definitions
  add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]['\w]*)(\h+['\w\[\]\(\),:\{\}\.]+)*\h+=\s 1:meta
  # Highlight curly braces, square brackets, and commas
  add-highlighter shared/haskell/code/ regex [\{\}\[\],] 0:operator
  # Highlight quasiquotes
  add-highlighter shared/haskell/quasiquote region \[\b[\w]['\w]*\| \|\] fill string
}

# Elm
hook global WinSetOption filetype=elm %{
  set-option window indentwidth 4
  set-option window formatcmd "elm-format --stdin"
  hook global -group format BufWritePre .* %{ format-buffer }
  # Improve bad syntax highlighting
  add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:function
  add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
}

# Rust
hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --emit stdout"
  hook global -group format BufWritePre .* %{ format-buffer }

# C++
hook global WinSetOption filetype=cpp %{
  set-option window formatcmd "clang-format"
  hook global -group format BufWritePre .* %{ format-buffer }
}

# Shell
hook global WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck -f gcc"
}

# Markdown
hook global WinSetOption filetype=markdown %{
  remove-hooks window markdown-indent
}

# Git
hook global WinSetOption filetype=git-commit %{
  remove-highlighter global/column
  add-highlighter window/ column 51 default,black
  add-highlighter window/ column 73 default,black
}
hook global WinCreate git-revise-todo %{
  set-option window filetype git-rebase
  strip-whitespace
}

# Docker
hook global WinCreate Dockerfile.* %{
  set-option window filetype dockerfile
}

# Prettier
hook global WinSetOption filetype=(javascript|typescript|vue|css|scss|less|json|markdown|yaml) %{
  set-option window formatcmd "prettier --stdin --stdin-filepath=${kak_buffile}"
}

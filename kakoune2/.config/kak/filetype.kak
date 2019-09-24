hook window WinSetOption filetype=haskell %{
  set-option window lintcmd "hlint"
  set-option window formatcmd "sort-imports"
  # add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]\w*)\s+::\s 1:function
}

hook window WinSetOption filetype=elm %{
  set-option window formatcmd "elm-format --stdin"
  hook window -group format BufWritePre .* format
  # add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:function
  # add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  # add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
}

hook window WinSetOption filetype=rust %{
  set-option window formatcmd "rustfmt --emit stdout"
  hook window -group format BufWritePre .* format
}

hook window WinSetOption filetype=cpp %{
  set-option window formatcmd "clang-format"
  hook window -group format BufWritePre .* format
}

hook window WinSetOption filetype=(javascript|typescript|vue|css|scss|less|json|markdown|yaml) %{
  set-option window formatcmd "prettier --stdin --stdin-filepath=${kak_buffile}"
}

hook window WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck -f gcc"
}

hook window WinSetOption filetype=markdown %{
  remove-hooks window markdown-indent
}

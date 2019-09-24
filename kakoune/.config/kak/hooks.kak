hook global WinCreate .* %{
  # add-highlighter global/ show-matching
  # add-highlighter global/ regex \b(TODO|FIXME|NOTE)\b 0:default+r
  # add-highlighter global/ show-whitespaces -lf " " -spc " "
  # add-highlighter global/ regex \h+$ 0:default,red
  git show-diff
}

hook buffer BufWritePost .* %{
  git update-diff
}

hook window InsertBegin .* %{
  remove-highlighter window/trailing-whitespace
}

hook window InsertEnd .* %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:default,red
}

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

define-command disable-autolint -docstring "Disable auto-lint" %{
  lint-disable
  unset-option window lintcmd
  remove-hooks window lint
}

define-command disable-autoformat -docstring "Disable auto-format" %{
  unset-option window formatcmd
  remove-hooks window format
}

hook buffer WinSetOption filetype=.* %{
  disable-autoformat
  disable-autolint
}

hook window WinSetOption filetype=haskell %{
  set-option window lintcmd "hlint"
  set-option window formatcmd "sort-imports"
  add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]\w*)\s+::\s 1:function
  lint-enable
  lint
}

# hook global WinSetOption filetype=purescript %{
# }

hook window WinSetOption filetype=elm %{
  set-option window formatcmd "elm-format --stdin"
  hook window -group format BufWritePre .* format
  # add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:function
  # add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  # add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
}

hook window WinSetOption filetype=rust %{
  set-option window makecmd "cargo build"
  set-option window formatcmd "rustfmt --emit stdout"
  hook window -group format BufWritePre .* format
}

hook window WinSetOption filetype=cpp %{
  set-option window formatcmd "clang-format"
  hook window -group format BufWritePre .* format
  clang-enable-autocomplete
  clang-enable-diagnostics
}

hook window WinSetOption filetype=(javascript|typescript|vue|css|scss|less|json|markdown|yaml) %{
  set-option window formatcmd "prettier --stdin --stdin-filepath=${kak_buffile}"
}

hook buffer WinSetOption filetype=(html|xml) %{
  set-option -add buffer auto_pairs < >
}

hook buffer WinSetOption filetype=markdown %{
  set-option -add buffer auto_pairs _ _ * *
}

hook window WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck -f gcc"
  hook window -group lint BufWritePost .* lint
  lint-enable
  lint
}

hook window WinSetOption filetype=markdown %{
  remove-hooks window markdown-indent
}

hook global WinCreate .* %{
  # Line numbers
  # add-highlighter window/number-lines number-lines -hlcursor -separator ' ' -relative
  # Highlight matching pairs
  add-highlighter global/ show-matching
  # Highlight TODO comments
  add-highlighter global/ regex \b(TODO|FIXME|XXX|NOTE)\b 0:default+r
}

hook global InsertCompletionShow .* %{
  map window insert <tab> <c-n>
  map window insert <s-tab> <c-p>
}

hook global InsertCompletionHide .* %{
  unmap window insert <tab> <c-n>
  unmap window insert <s-tab> <c-p>
}

define-command disable-autolint -docstring 'Disable auto-lint' %{
  lint-disable
  unset-option window lintcmd
  remove-hooks window lint
}

define-command disable-autoformat -docstring 'Disable auto-format' %{
  unset-option window formatcmd
  remove-hooks window format
}

hook global WinSetOption filetype=.* %{
  disable-autoformat
  disable-autolint
  hook window -group format BufWritePre .* %{
    try %{ execute-keys -draft \%s\h+$<ret>d }
  }
}

hook global WinSetOption filetype=haskell %{
  set-option window lintcmd 'hlint'
  set-option window formatcmd 'brittany'
  hook window -group lint BufWritePost .* lint
  # add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]\w*)\s+::\s 1:type
  add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]\w*)\s+::\s 1:function
  lint-enable
  lint
}

hook global WinSetOption filetype=elm %{
  set-option window formatcmd 'elm-format --stdin'
  hook window -group format BufWritePre .* format
  add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:type
  add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
}


hook global WinSetOption filetype=cpp %{
  set-option window formatcmd 'clang-format'
  hook window -group format BufWritePre .* format
  clang-enable-autocomplete
  clang-enable-diagnostics
}

hook global WinSetOption filetype=(javascript|typescript|css|scss|json|markdown|yaml) %{
  set-option window formatcmd 'prettier --stdin-filepath=${kak_buffile}'
  hook window -group format BufWritePre .* format
}

hook global WinSetOption filetype=markdown %{
  set-option -add buffer auto_pairs _ _ * *
}

hook global WinSetOption filetype=sh %{
  set-option window lintcmd 'shellcheck -f gcc'
  hook window -group lint BufWritePost .* lint
  lint-enable
  lint
}

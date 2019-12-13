provide-module "user_filetype" %{

# Haskell
hook global WinSetOption filetype=haskell %{
  set-option window lintcmd "hlint"
  # set-option window lintcmd "sleep 0.5; ghcid-format /tmp/ghcid"
  hook window -group lint BufWritePost .* %{ lint }
  lint-enable
  # set-option window formatcmd "ormolu -o -XTypeApplications"
  set-option window formatcmd "sort-imports"
  define-snippet window "forall" "∀"
  define-snippet window "lang" "{-# LANGUAGE OverloadedStrings #-}"
  define-snippet window "opt" "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
}

# PureScript
hook global WinSetOption filetype=purescript %{
  # Highlight function name in type signatures
  add-highlighter shared/purescript/code/ regex ^\h*(?:(?:where|let)\h+)?([_a-z]['\w]*)\s+::\s 1:meta
  # Replace 'forall' with '∀'
  add-highlighter shared/purescript/code/ regex ∀ 0:keyword
  define-snippet window "forall" "∀"
  set-option window comment_line "--"
  set-option window comment_block_begin "{-"
  set-option window comment_block_end "-}"
}

# Elm
hook global WinSetOption filetype=elm %{
  set-option window indentwidth 4
  set-option window formatcmd "elm-format --stdin"
  hook window -group format BufWritePre .* %{ format-buffer }
  # Improve bad syntax highlighting
  # add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:function
  # add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  # add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
}

# Rust
hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --emit stdout"
  hook window -group format BufWritePre .* %{ format-buffer }
}

# C++
hook global WinSetOption filetype=cpp %{
  set-option window formatcmd "clang-format"
  hook window -group format BufWritePre .* %{ format-buffer }
}

# Shell
hook global WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck -f gcc"
  hook window -group lint BufWritePost .* %{ lint }
  lint-enable
}

# Fish
hook global WinSetOption filetype=fish %{
  # set-option window lintcmd "fish --no-execute"
  set-option window indentwidth 4
}

# Markdown
hook global WinSetOption filetype=markdown %{
  remove-hooks window markdown-indent
  map window filetype "=" "|fmt -w 80<ret>" -docstring "Wrap to 80 columns"
  add-highlighter shared/markdown/comment region -recurse <!-- <!-- --> fill comment
  set-option window comment_block_begin "<!-- "
  set-option window comment_block_end " -->"
}

# Git
hook global WinSetOption filetype=git-commit %{
  add-highlighter window/ column 51 default,black
  add-highlighter window/ column 73 default,black
  define-snippet window "date" '%sh{date +%Y-%m-%d}'
}
hook global WinCreate git-revise-todo %{
  set-option window filetype git-rebase
  strip-whitespace
}

# Docker
hook global WinCreate Dockerfile.* %{
  set-option window filetype dockerfile
}

# Makefile
hook global WinSetOption filetype=makefile %{
  set-option window aligntab true
  set-option window indentwidth 0
}

# SQL
hook global WinSetOption filetype=sql %{
  hook window WinClose .* %{ psql-disable }
  map window filetype "s" ": query-selection<ret>" -docstring "Query selection"
  map window filetype "b" ": query-buffer<ret>" -docstring "Query buffer"
  # map window filetype "c" ": nop %%sh{ echo '' > /tmp/$kak_opt_psql_tmpfile }<ret>" -docstring "Clear query history"
  psql-enable
}

# Kakoune
hook global WinSetOption filetype=kak %{
  add-highlighter shared/kakrc/code/ regex \bdefine-snippet\b 0:keyword
}
hook global BufCreate \*scratch\* %{
  execute-keys '%d'
}

# Prettier
hook global WinSetOption filetype=markdown %{ set-option window formatcmd "prettier --stdin --parser markdown" }
hook global WinSetOption filetype=json %{ set-option window formatcmd "prettier --stdin --parser json" }
hook global WinSetOption filetype=yaml %{ set-option window formatcmd "prettier --stdin --parser yaml" }
hook global WinSetOption filetype=javascript %{ set-option window formatcmd "prettier --stdin --parser javascript" }
hook global WinSetOption filetype=typescript %{ set-option window formatcmd "prettier --stdin --parser typescript" }
hook global WinSetOption filetype=css %{ set-option window formatcmd "prettier --stdin --parser css" }
hook global WinSetOption filetype=scss %{ set-option window formatcmd "prettier --stdin --parser scss" }
hook global WinSetOption filetype=less %{ set-option window formatcmd "prettier --stdin --parser less" }

}

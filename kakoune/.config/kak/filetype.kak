# Haskell
hook global WinSetOption filetype=haskell %{
  set-option window lintcmd "hlint"
  hook window -group lint BufWritePost .* %{ lint }
  lint-enable
  set-option window formatcmd "sort-imports"
  # Highlight function name in type signatures
  add-highlighter shared/haskell/code/ regex ^\h*(?:(?:where|let|default)\h+)?([_a-z]['\w]*)\s+::\s 1:meta
  # Highlight quasiquotes
  add-highlighter shared/haskell/quasiquote region \[\b[\w]['\w]*\| \|\] fill string
  map window filetype "l" "gkO{-# LANGUAGE ChangeMe #-}<esc>bbe" -docstring "Insert LANGUAGE pragma"
  map window filetype "o" "gk]po{-# OPTIONS_GHC -Wchange-me #-}<esc>bb<a-e>" -docstring "Insert OPTIONS_GHC pragma"
}

# Elm
hook global WinSetOption filetype=elm %{
  set-option window indentwidth 4
  set-option window formatcmd "elm-format --stdin"
  hook window -group format BufWritePre .* %{ format-buffer }
  # Improve bad syntax highlighting
  add-highlighter shared/elm/code/ regex ^\h*(?:let\h+)?([_a-z]\w*)\s+:\s 1:function
  add-highlighter shared/elm/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.a-z]) 0:variable
  add-highlighter shared/elm/code/ regex (?<![~<=>|!?/.@$*&#%+\^\-\\])[~<=>|!?/.@$*&#%+\^\-\\]+ 0:operator
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
}

# Git
hook global WinSetOption filetype=git-commit %{
  add-highlighter window/ column 51 default,black
  add-highlighter window/ column 73 default,black
  map window filetype "d" "|date '+%%Y-%%m-%%d'<ret>;" -docstring "Insert ISO 8601 date"
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
  declare-option -docstring "Postgres database" str postgres_database
  declare-option -docstring "Postgres user" str postgres_user
  declare-option -docstring "Postgres host" str postgres_host
  declare-option -docstring "Postgres port" int postgres_port
  declare-option -hidden str postgres_command
  declare-option -hidden str postgres_response
  set-option window postgres_database "vetpro"
  set-option window postgres_user "postgres"
  set-option window postgres_host "localhost"
  set-option window postgres_port 5432
  set-option window postgres_command "psql -A -d %opt{postgres_database} -U %opt{postgres_user} -h %opt{postgres_host} -p %opt{postgres_port}"

  declare-option -hidden str psql_tmpfile
  set-option window psql_tmpfile %sh{ mktemp /tmp/kakoune_psql.XXXX }
  tmux-terminal-horizontal kak -s "%val{session}-psql" -e "edit %opt{psql_tmpfile}; set-option window autoreload yes"

  hook window WinClose .* %{ nop %sh{
    kak -c "${kak_session}-psql" -e "kill!"
    # rm "/tmp/$kak_opt_psql_tmpfile"
  }}

  define-command -docstring "query-buffer" \
  query-buffer %{
    execute-keys -draft "%%<a-|>%opt{postgres_command} >> %opt{psql_tmpfile} 2>&1<ret>"
  }
  define-command -docstring "query-selection" \
  query-selection %{
    execute-keys -itersel -draft "<a-|>%opt{postgres_command} >> %opt{psql_tmpfile} 2>&1<ret>"
  }

  map window filetype "s" ": query-selection<ret>" -docstring "Query selection"
  map window filetype "b" ": query-buffer<ret>" -docstring "Query buffer"
  # map window filetype "c" ": nop %%sh{ echo '' > /tmp/$kak_opt_psql_tmpfile }<ret>" -docstring "Clear query history"
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

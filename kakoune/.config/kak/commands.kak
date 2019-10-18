# Extend line-based selections
define-command -docstring "extend-line-down: extend selection with line down" \
extend-line-down -params 1 %{
  execute-keys "<a-:>%arg{1}X"
}

define-command -docstring "extend-line-up: extend selection with line up" \
extend-line-up -params 1 %{
  execute-keys "<a-:><a-;>%arg{1}K<a-;>"
  try %{
    execute-keys -draft ";<a-K>\n<ret>"
    execute-keys "X"
  }
  execute-keys "<a-;><a-X>"
}

# Soft wrapping
define-command -docstring "softwrap-enable: enable soft wrapping" \
softwrap-enable %{
  add-highlighter window/softwrap wrap
}

define-command -docstring "softwrap-disable: disable soft wrapping" \
softwrap-disable %{
  remove-highlighter window/softwrap
}

# tmux splits
define-command -docstring "split <filename>: open file in horizontal tmux split" \
split -params 0.. -file-completion %{
  # tmux-terminal-vertical sh -c "kak -c %val{session} %arg{@}; fish"
  tmux-terminal-vertical kak -c %val{session} %arg{@}
}
alias global sp split

define-command -docstring "vsplit <filename>: open file in vertical tmux split" \
vsplit -params 0.. -file-completion %{
  # tmux-terminal-horizontal sh -c "kak -c %val{session} %arg{@}; fish"
  tmux-terminal-horizontal kak -c %val{session} %arg{@}
}
alias global vs vsplit

define-command -docstring "horizontally: run command in horizontal split" \
horizontally -params 0.. -command-completion %{
  tmux-terminal-vertical kak -c %val{session} -e "%arg{@}"
}
alias global horiz horizontally

define-command -docstring "vertically: run command in vertical split" \
vertically -params 0.. -command-completion %{
  tmux-terminal-horizontal kak -c %val{session} -e "%arg{@}"
}
alias global vert vertically

# Evaluate
define-command -docstring "evaluate-buffer: evaluate buffer commands as if entered by user" \
evaluate-buffer %{
  execute-keys -draft "%: <c-r>.<ret>"
}

define-command -docstring "evaluate-selection: evaluate selection commands as if entered by user" \
evaluate-selection %{
  execute-keys -itersel -draft ": <c-r>.<ret>"
}

# Other
define-command -docstring "strip-whitespace: strip trailing whitespace" \
strip-whitespace %{
  execute-keys -draft "%%s\h+$<ret>d"
}

define-command -docstring "filetype: change filetype" \
filetype -params 1 %{
  set-option window filetype %arg{1}
}

# PostgreSQL
define-command -docstring "psql-enable" \
psql-enable %{
  declare-option -docstring "Postgres database" str postgres_database
  declare-option -docstring "Postgres user" str postgres_user
  declare-option -docstring "Postgres host" str postgres_host
  declare-option -docstring "Postgres port" int postgres_port
  declare-option -hidden str psql_tmpfile
  set-option window postgres_database "vetpro"
  set-option window postgres_user "postgres"
  set-option window postgres_host "localhost"
  set-option window postgres_port 5432
  set-option window psql_tmpfile %sh{ mktemp /tmp/kakoune_psql.XXXX }

  define-command -docstring "query-selection" \
  query-selection %{
    execute-keys -itersel -draft "<a-|>psql -A -d %opt{postgres_database} -U %opt{postgres_user} -h %opt{postgres_host} -p %opt{postgres_port} >> %opt{psql_tmpfile} 2>&1<ret>"
  }

  define-command -docstring "query-buffer" \
  query-buffer %{
    execute-keys -draft "%%: query-selection<ret>"
  }

  # tmux-terminal-horizontal kak -s "%val{session}-psql" -e "edit %opt{psql_tmpfile}; set-option window autoreload yes; nop %sh{ tmux select-pane -t .! }"
  tmux-terminal-horizontal kak -s "%val{session}-psql" -e "edit %opt{psql_tmpfile}; set-option window autoreload yes"

  hook window WinClose .* %{ psql-disable }
}

define-command -docstring "psql-disable" \
psql-disable %{
  unset-option window postgres_database
  unset-option window postgres_user
  unset-option window postgres_host
  unset-option window postgres_port
  unset-option window psql_tmpfile
  nop %sh{
    kak -c "${kak_session}-psql" -e "kill!"
    rm -f "/tmp/$kak_opt_psql_tmpfile"
    echo "$kak_opt_psql_tmpfile" > /tmp/foo
    rm -f "/tmp/"
  }
}


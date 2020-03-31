define-command -docstring "Source configuration lazily (after NormalIdle)" \
defer -params 1 %{
  hook -once global NormalIdle .* %arg{1}
}

# Remove buffer when quitting from Kakoune client
define-command -docstring "quit-client" -override \
quit-client -params 0..1 %{
  # TODO: Delete all buffers that are not open in other windows
  try %{
    evaluate-commands %sh{
      if echo "$kak_bufname" | grep -qvE '^\*.+\*$'; then
        echo "delete-buffer"
      fi
    }
    quit %arg{@}
  } catch %{ evaluate-commands %sh{
    if [ "$kak_modified" = "true" ]; then
      echo "echo -markup '{Error}buffer is modified{Default}'"
    else
      echo "echo -markup '{Error}%val{error}{Default}'"
    fi
  }}
}
# alias global q quit-client

# Snippets
define-command -docstring "add-snippet" \
add-snippet -params 3 %{ evaluate-commands %sh{
  scope=$1
  snippet=$2
  expansion=$3
  printf "%s" "
  set-option -add $scope static_words '$2'
  set-option -add $scope snippets '$2' '$3'
  "
}}

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

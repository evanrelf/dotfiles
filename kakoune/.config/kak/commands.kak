# define-command -docstring "Source configuration lazily (after NormalIdle)" \
# defer -params 1 %{
#   hook -once global NormalIdle .* %arg{1}
# }

# Snippets
define-command -docstring "add-snippet" \
add-snippet -params 3 %{ evaluate-commands %sh{
  scope=$1
  snippet=$2
  expansion=$3
  printf "%s" "
  try %{
    set-option -add $scope snippets '$2' '$3'
    set-option -add $scope static_words '$2'
  } catch %{
    echo -debug 'add-snippet: Unable to add snippet because snippets.kak not loaded ($snippet to $expansion)'
  }
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
  tmux-terminal-vertical kak -c %val{session} %arg{@}
}
alias global sp split

define-command -docstring "vsplit <filename>: open file in vertical tmux split" \
vsplit -params 0.. -file-completion %{
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

# Share clipboard with OS
evaluate-commands %sh{
  case "$(uname)" in
    "Darwin")
      copy="pbcopy"
      paste="pbpaste"
      ;;
    "Linux")
      copy="wl-copy || xclip"
      paste="wl-paste || xclip -o"
      ;;
    *)
      copy="false"
      paste="false"
      ;;
  esac
  printf "%s" "
  define-command yank -docstring 'Yank to clipboard' %{
    execute-keys '<a-|>$copy<ret>'
  }
  alias global y yank

  define-command paste-after -docstring 'Paste after from clipboard' %{
    execute-keys '<a-!>$paste<ret>'
  }
  alias global paste paste-after
  alias global p paste-after

  define-command paste-before -docstring 'Paste before from clipboard' %{
    execute-keys '!$paste<ret>'
  }
  alias global P paste-before

  define-command paste-replace -docstring 'Paste replace from clipboard' %{
    execute-keys '|$paste<ret>'
  }
  alias global replace paste-replace
  alias global R paste-replace
  "
}
# # Daemon helpers
# define-command -hidden daemon-init %{
#   rename-session "daemon-%sh{shuf -i 1-1000 -n 1}"
#   define-command -hidden client-init %{ rename-client "client-%val{session}" }
#   define-command -hidden write-kill -params 0.. %{ write %arg{@}; kill }
#   define-command -hidden write-kill-all -params 0.. %{ write-all %arg{@}; kill }
#   define-command -hidden write-kill-force -params 0.. %{ write %arg{@}; kill! }
#   define-command -hidden write-kill-all-force -params 0.. %{ write-all %arg{@}; kill! }
#   alias global q kill
#   alias global wq write-kill
#   alias global wqa write-kill-all
#   alias global waq write-kill-all
#   alias global q! kill!
#   alias global wq! write-kill-force
#   alias global wqa! write-kill-all-force
#   alias global waq! write-kill-all-force
# }


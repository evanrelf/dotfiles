declare-option -hidden str clipboard_copy "<esc>: fail 'No clipboard config'<ret>"
declare-option -hidden str clipboard_paste "<esc>: fail 'No clipboard config'<ret>"

hook -once global NormalIdle .* %{
  evaluate-commands %sh{
    case "$(uname)" in
      "Darwin")
        echo "set-option global clipboard_copy 'pbcopy'"
        echo "set-option global clipboard_paste 'pbpaste'"
        ;;
      "Linux")
        echo "set-option global clipboard_copy 'wl-copy || xclip || tmux load-buffer -'"
        echo "set-option global clipboard_paste 'wl-paste || xclip -o || tmux save-buffer -'"
        ;;
    esac
  }
}

define-command yank -docstring 'Yank to clipboard' %{
  execute-keys "<a-|>%opt{clipboard_copy}<ret>"
}
alias global y yank

define-command paste-after -docstring 'Paste after from clipboard' %{
  execute-keys "<a-!>%opt{clipboard_paste}<ret>"
}
alias global paste paste-after
alias global p paste-after

define-command paste-before -docstring 'Paste before from clipboard' %{
  execute-keys "!%opt{clipboard_paste}<ret>"
}
alias global P paste-before

define-command paste-replace -docstring 'Paste replace from clipboard' %{
  execute-keys "|%opt{clipboard_paste}<ret>"
}
alias global replace paste-replace
alias global R paste-replace

map global "user" "y" ": yank<ret>" -docstring "Yank"
map global "user" "p" ": paste-after<ret>" -docstring "Paste after"
map global "user" "P" ": paste-before<ret>" -docstring "Paste before"
map global "user" "R" ": paste-replace<ret>" -docstring "Paste replace"

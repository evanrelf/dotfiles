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

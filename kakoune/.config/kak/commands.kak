define-command -docstring 'Strip trailing whitespace' -params 0 strip-whitespace %{
  execute-keys -draft \%s\h+$<ret>d
}

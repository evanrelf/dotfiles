define-command -docstring 'Strip trailing whitespace' -params 0 strip-whitespace %{
  execute-keys -draft \%s\h+$<ret>d
}

define-command -docstring 'Open file in vertical split' -params 0..1 -file-completion vsplit %{
  iterm-new-vertical edit %arg{1}
}
alias global vs vsplit

define-command -docstring 'Open file in horizontal split' -params 0..1 -file-completion split %{
  iterm-new-horizontal edit %arg{1}
}
alias global sp split

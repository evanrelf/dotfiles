map global normal '/' '/(?i)'
map global normal '?' '?(?i)'
map global normal '<a-/>' '<a-/>(?i)'
map global normal '<a-?>' '<a-?>(?i)'
map global normal '<space>' ','
map global normal ',' '<space>'
map global normal '#' ': comment-line<ret>'

map global user 'y' '<a-|>pbcopy<ret>' -docstring 'Yank to system clipboard'
map global user 'p' '!pbpaste<ret>' -docstring 'Paste from system clipboard'

alias global 'split' 'iterm-new-horizontal'
alias global 'vsplit' 'iterm-new-vertical'

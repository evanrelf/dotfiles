map global normal / /(?i)
map global normal <space> ,
map global normal , <space>
map global normal '#' ': comment-line<ret>'
map global normal '=' ': format<ret>'

map global user y ': nop %sh{ printf "$kak_selection" | pbcopy }<ret>' -docstring 'Yank to system clipboard'
map global user s ': auto-pairs-surround<ret>' -docstring 'Surround'

map global normal <space> ','
map global normal ,       '<space>'
map global normal /       '/(?i)'
map global normal ?       '?(?i)'
map global normal <a-/>   '<a-/>(?i)'
map global normal <a-?>   '<a-?>(?i)'
map global normal '#'     ': comment-line<ret>'

map global user w ': write<ret>' -docstring 'Write buffer'
map global user q ': delete-buffer<ret>: quit<ret>'
map global user y '<a-|>pbcopy<ret>' -docstring 'Yank to system clipboard'
map global user p '!pbpaste<ret>' -docstring 'Paste from system clipboard'
map global user '=' ': format<ret>' -docstring 'Format buffer'

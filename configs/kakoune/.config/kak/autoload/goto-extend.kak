# Put "goto (extend to)" mappings under "goto" mode, freeing up `G` key
map global "goto" "H" "<esc>Gh" -docstring "line begin (extend)"
map global "goto" "L" "<esc>Gl" -docstring "line end (extend)"
map global "goto" "K" "<esc>Gk" -docstring "buffer top (extend)"
map global "goto" "J" "<esc>Gj" -docstring "buffer bottom (extend)"
map global "goto" "I" "<esc>Gi" -docstring "line non blank start (extend)"
map global "goto" "E" "<esc>Ge" -docstring "buffer end (extend)"
map global "goto" "T" "<esc>Gt" -docstring "window top (extend)"
map global "goto" "C" "<esc>Gc" -docstring "window center (extend)"
map global "goto" "B" "<esc>Gb" -docstring "window bottom (extend)"

# Disable `G` key
map global "normal" "G" ": fail 'Use g'<ret>"

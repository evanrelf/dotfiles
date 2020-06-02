# Temporary
map global user "<space>" "<a-i>p|stylish-haskell<ret>:wq"

# Use q for backwards word movement
map global normal "q" "b"
map global normal "Q" "B"
map global normal "<a-q>" "<a-b>"
map global normal "<a-Q>" "<a-B>"

# Use quotes for macros
map global normal "'" "q"
map global normal '"' "Q"

# Space is my leader
map global normal "," "<space>"
map global normal "<space>" ","
map global normal "<a-,>" "<a-space>"
map global normal "<a-space>" "<a-,>"

# Make search case-insensitive by default
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"

# Use alternative registers for K and S stuff
map global normal "<a-k>" '"k<a-k>'
map global normal "<a-K>" '"k<a-K>'
map global normal "s" '"ss'
map global normal "S" '"sS'
map global normal "<a-s>" '"s<a-s>'
map global normal "<a-S>" '"s<a-S>'

# Drop selection when entering insert mode
map global normal "i" ";i"
map global normal "a" ";a"

# Commenting
map global normal "#" ": comment-line<ret>"
map global normal "<a-#>" ": comment-block<ret>"

# Allow selecting by line in both directions
map global normal "x" ": extend-line-down %%val{count}<ret>"
map global normal "X" ": extend-line-up %%val{count}<ret>"

# Center viewport when jumping around
map global normal "<c-i>" "<c-i>vc"
map global normal "<c-o>" "<c-o>vc"
map global normal "n" "nvc"
map global normal "N" "Nvc"
map global normal "<a-n>" "<a-n>vc"
map global normal "<a-N>" "<a-N>vc"

# User mode
map global user "/" ": execute-keys /<ret>\Q\E<left><left>" -docstring "Search without regex"
map global user "i" ': execute-keys %val{count}o<lt>backspace><ret>' -docstring "Insert with count"

# Disabled
map global normal "<a-h>" ": echo -markup '{Error}Use Gh{Default}'<ret>" -docstring "Use Gh"
map global normal "<a-l>" ": echo -markup '{Error}Use Gl{Default}'<ret>" -docstring "Use Gl"
map global normal "<a-H>" ": echo -markup '{Error}Use Gh{Default}'<ret>" -docstring "Use Gh"
map global normal "<a-L>" ": echo -markup '{Error}Use Gl{Default}'<ret>" -docstring "Use Gl"
map global goto "g" "<esc>: echo -markup '{Error}Use gk{Default}'<ret>" -docstring "Use gk"
map global view "v" "<esc>: echo -markup '{Error}Use vc{Default}'<ret>" -docstring "Use vc"
map global normal "b" ": echo -markup '{Error}Use q{Default}'<ret>" -docstring "Use q"
map global normal "B" ": echo -markup '{Error}Use Q{Default}'<ret>" -docstring "Use Q"
map global normal "<a-b>" ": echo -markup '{Error}Use a-q{Default}'<ret>" -docstring "Use ⌥q"
map global normal "<a-B>" ": echo -markup '{Error}Use a-Q{Default}'<ret>" -docstring "Use ⌥Q"
map global insert "<c-n>" "<a-;>: echo -markup '{Error}Use tab{Default}'<ret>" -docstring "Use ⌥Q"
map global insert "<c-p>" "<a-;>: echo -markup '{Error}Use s-tab{Default}'<ret>" -docstring "Use ⌥Q"
map global user "y" ": echo -markup '{Error}Use yank{Default}'<ret>" -docstring "Use yank"
map global user "p" ": echo -markup '{Error}Use paste-before{Default}'<ret>" -docstring "Use paste-before"
map global user "P" ": echo -markup '{Error}Use paste-after{Default}'<ret>" -docstring "Use paste-after"
map global user "R" ": echo -markup '{Error}Use paste-replace{Default}'<ret>" -docstring "Use paste-replace"

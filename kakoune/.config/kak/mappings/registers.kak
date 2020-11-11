# Use alternative registers for K and S stuff
map global "normal" "<a-k>" '"k<a-k>'
map global "normal" "<a-K>" '"k<a-K>'
map global "normal" "s" '"ss'
map global "normal" "S" '"sS'
map global "normal" "<a-s>" '"s<a-s>'
map global "normal" "<a-S>" '"s<a-S>'

# Clear search register
map global "normal" "<esc>" ": set-register / ''<ret>"

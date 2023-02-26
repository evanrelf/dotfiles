# Highlight matches
add-highlighter global/search-matches dynregex "%%reg{/}" 0:black,bright-white+f

# Make search case-insensitive by default
map global "normal" "/" "/(?i)"
map global "normal" "?" "?(?i)"
map global "normal" "<a-/>" "<a-/>(?i)"
map global "normal" "<a-?>" "<a-?>(?i)"

# Center viewport after jumping
map global "normal" "n" "nvc"
map global "normal" "N" "Nvc"
map global "normal" "<a-n>" "<a-n>vc"
map global "normal" "<a-N>" "<a-N>vc"
map global "normal" "<c-i>" "<c-i>vc"
map global "normal" "<c-o>" "<c-o>vc"

# Don't use `/` register for split and keep
map global "normal" "s" '"ss'
map global "normal" "S" '"sS'
map global "normal" "<a-k>" '"k<a-k>'
map global "normal" "<a-K>" '"k<a-K>'

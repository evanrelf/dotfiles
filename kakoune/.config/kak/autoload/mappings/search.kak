# Make search case-insensitive by default
map global "normal" "/" "/(?i)"
map global "normal" "?" "?(?i)"
map global "normal" "<a-/>" "<a-/>(?i)"
map global "normal" "<a-?>" "<a-?>(?i)"

# Center viewport when jumping around
map global "normal" "<c-i>" "<c-i>vc"
map global "normal" "<c-o>" "<c-o>vc"
map global "normal" "n" "nvc"
map global "normal" "N" "Nvc"
map global "normal" "<a-n>" "<a-n>vc"
map global "normal" "<a-N>" "<a-N>vc"

# Discourage non-mnemonic keys
map global "normal" "<a-h>" ": fail 'Use Gh'<ret>"
map global "normal" "<a-l>" ": fail 'Use Gl'<ret>"
map global "normal" "<a-H>" ": fail 'Use Gh'<ret>"
map global "normal" "<a-L>" ": fail 'Use Gl'<ret>"
map global "goto" "g" "<esc>: fail 'Use gk'<ret>"
map global "view" "v" "<esc>: fail 'Use vc'<ret>"
map global "insert" "<c-n>" "<a-;>: fail 'Use tab'<ret>"
map global "insert" "<c-p>" "<a-;>: fail 'Use s-tab'<ret>"

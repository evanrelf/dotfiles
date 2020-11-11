# Discourage non-mnemonic keys
map global "normal" "<a-h>" ": fail 'Use Gh'<ret>" -docstring "Use Gh"
map global "normal" "<a-l>" ": fail 'Use Gl'<ret>" -docstring "Use Gl"
map global "normal" "<a-H>" ": fail 'Use Gh'<ret>" -docstring "Use Gh"
map global "normal" "<a-L>" ": fail 'Use Gl'<ret>" -docstring "Use Gl"
map global "goto" "g" "<esc>: fail 'Use gk'<ret>" -docstring "Use gk"
map global "view" "v" "<esc>: fail 'Use vc'<ret>" -docstring "Use vc"
map global "insert" "<c-n>" "<a-;>: fail 'Use tab'<ret>" -docstring "Use tab"
map global "insert" "<c-p>" "<a-;>: fail 'Use s-tab'<ret>" -docstring "Use s-tab"

# # Swap : and ;
# map global "normal" ":" ";"
# map global "normal" ";" ":"

# Use q for backwards word movement
map global "normal" "q" "b"
map global "normal" "Q" "B"
map global "normal" "<a-q>" "<a-b>"
map global "normal" "<a-Q>" "<a-B>"
map global "normal" "b" "q"
map global "normal" "B" "Q"
map global "normal" "<a-b>" ": fail 'Use a-q'<ret>" -docstring "Use a-q"
map global "normal" "<a-B>" ": fail 'Use a-Q'<ret>" -docstring "Use a-Q"

# Insert with count
map global "user" "i" ': execute-keys %val{count}o<lt>backspace><ret>' -docstring "Insert with count"

# Comment out line
map global "normal" "#" ": comment-line<ret>"
map global "normal" "<a-#>" ": comment-block<ret>"

# Use <c-n> and <c-p> in command mode
map global "prompt" "<c-n>" "<down>"
map global "prompt" "<c-p>" "<up>"

# Format buffer
map global "user" "=" ": format-buffer<ret>" -docstring "Format buffer"

# Plugins are provided by Nix

# evanrelf/primer.kak
colorscheme primer
declare-option str column_color "rgb:f6f8fa"

# evanrelf/number-toggle.kak
set-option global number_toggle_params -hlcursor

# evanrelf/byline.kak
require-module "byline"

# evanrelf/reselect.kak
require-module "reselect"
map global "normal" "i" ": reselect<ret>;i"
map global "normal" "a" ": reselect-a<ret>"

# alexherbo2/replace-mode.kak
require-module "replace-mode"
map global "normal" "<a-r>" ": enter-replace-mode<ret>"

# listentolist/kakoune-fandt
# require-module "fandt"

# h-youhei/kakoune-surround
map global "user" "s" ": enter-user-mode surround<ret>" -docstring "Surround mode"
declare-user-mode "surround"
map global "surround" "s" ": select-surround<ret>" -docstring "Select surround"
map global "surround" "a" ": surround<ret>" -docstring "Add surround"
map global "surround" "r" ": change-surround<ret>" -docstring "Replace surround"
map global "surround" "d" ": delete-surround<ret>" -docstring "Delete surround"
map global "surround" "t" ": enter-user-mode surround-tag<ret>" -docstring "Surround tag"
declare-user-mode "surround-tag"
map global "surround-tag" "t" ": select-surrounding-tag<ret>" -docstring "Select surrounding tag"
map global "surround-tag" "a" ": surround-with-tag<ret>" -docstring "Add surrounding tag"
map global "surround-tag" "r" ": change-surrounding-tag<ret>" -docstring "Replace surrounding tag"
map global "surround-tag" "d" ": delete-surrounding-tag<ret>" -docstring "Delete surrounding tag"

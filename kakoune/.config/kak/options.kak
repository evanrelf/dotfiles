# Appearance
colorscheme palenight
add-highlighter global/ show-matching
add-highlighter global/ dynregex "%%reg{/}" 0:+u
add-highlighter global/column column 81 default,black
add-highlighter global/ regex \b(TODO|FIXME|XXX|NOTE)\b 0:default+r
# Modified colors for palenight theme
face global PrimaryCursor rgb:292d3e,rgb:ffcb6b+bfg
face global PrimarySelection rgb:292d3e,rgb:d7dae0+bfg
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true

# Options
set-option global indentwidth 2
set-option global grepcmd "rg --column --smart-case"
set-option global scrolloff 1,5
set-option global startup_info_version 20190701

# # Highlight current word
# declare-option -hidden regex currentword
# set-face global CurrentWord default,rgb:4a4a4a

# hook global NormalIdle .* %{
#   evaluate-commands -draft %{ try %{
#     execute-keys <space><a-i>w <a-k>\A[\w-]+\z<ret>
#     set-option buffer currentword "\b\Q%val{selection}\E\b"
#   } catch %{
#     set-option buffer currentword ""
#   } }
# }
# add-highlighter global/ dynregex "%%opt{currentword}" 0:CurrentWord

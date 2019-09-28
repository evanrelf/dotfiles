# Appearance
colorscheme palenight
add-highlighter global/column column 81 default,black
add-highlighter global/ regex \b(TODO|FIXME|NOTE)\b 0:default+r
# Modified colors for palenight theme
face global PrimaryCursor rgb:292d3e,rgb:ffcb6b+bfg
face global PrimarySelection rgb:292d3e,rgb:d7dae0+bfg
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true \
  ncurses_set_title=true

# Options
set-option global indentwidth 2
set-option global grepcmd "rg --column --smart-case"
set-option global scrolloff 1,5
set-option global startup_info_version 20190701

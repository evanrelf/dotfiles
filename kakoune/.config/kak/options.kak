provide-module "user_options" %{

# Appearance
colorscheme palenight
add-highlighter global/ show-matching
add-highlighter global/ dynregex "%%reg{/}" 0:+u
add-highlighter global/ column 81 default,rgb:2b3243
add-highlighter global/ regex \b(TODO|FIXME|XXX|NOTE)\b 0:default+r
# Modified colors for palenight theme
face global PrimaryCursor rgb:292d3e,rgb:ffcb6b+fg
face global PrimarySelection default,rgb:5c6071+g
face global SecondarySelection default,rgb:434758+g
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true

# Options
set-option global indentwidth 2
set-option global grepcmd "rg --column --smart-case"
set-option global scrolloff 1,5
set-option global startup_info_version 20191210

}

# Appearance
try %{
  colorscheme primer
  add-highlighter global/80 column 81 default,rgb:f6f8fa
}
# colorscheme challenger-deep
# add-highlighter global/80 column 81 default,rgb:252438
# colorscheme palenight
# set-face global PrimaryCursor rgb:292d3e,rgb:ffcb6b+fg
# set-face global PrimarySelection default,rgb:5c6071+g
# set-face global SecondarySelection default,rgb:434758+g
# add-highlighter global/ column 81 default,rgb:2c3344
add-highlighter global/ number-lines -hlcursor -relative
add-highlighter global/ show-matching
add-highlighter global/ dynregex "%%reg{/}" 0:+u
add-highlighter global/ regex "\b(TODO|FIXME|NOTE)\b" 0:default+r
# Options
# set-option global autoinfo command
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true

set-option global indentwidth 2
set-option global grepcmd "rg --column --smart-case"
set-option global startup_info_version 99999999
set-option global spell_lang "en-US"

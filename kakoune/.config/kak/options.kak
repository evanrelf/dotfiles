provide-module "user_options" %{

# Appearance
try %{
  colorscheme challenger-deep
  add-highlighter global/80 column 81 default,rgb:252438
} catch %{
  colorscheme palenight
  set-face global PrimaryCursor rgb:292d3e,rgb:ffcb6b+fg
  set-face global PrimarySelection default,rgb:5c6071+g
  set-face global SecondarySelection default,rgb:434758+g
  add-highlighter global/ column 81 default,rgb:2c3344
}
add-highlighter global/ number-lines -hlcursor
add-highlighter global/ show-matching
add-highlighter global/ dynregex "%%reg{/}" 0:+u
add-highlighter global/ regex "\b(TODO|FIXME|NOTE)\b" 0:default+r
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true

# Options
set-option global indentwidth 2
set-option global grepcmd "rg --column --smart-case"
set-option global startup_info_version 20191210

}

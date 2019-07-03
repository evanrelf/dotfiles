colorscheme solarized-dark
set-option global ui_options \
  ncurses_assistant=none \
  ncurses_enable_mouse=true \
  ncurses_set_title=true \
  #  ncurses_status_on_top=true \
set-option global aligntab false
set-option global tabstop 2
set-option global indentwidth 2
set-option global grepcmd 'rg --column --smart-case'
set-option global scrolloff 1,5
set-option global startup_info_version 20190120
set-option global idle_timeout 100
add-highlighter global/ column 81 default,black

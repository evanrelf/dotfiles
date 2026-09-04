set --global --export COLORTERM "$TERM"
set --global fish_color_command normal
set --global fish_color_keyword $fish_color_command
set --global fish_color_param $fish_color_command
set --global fish_color_option $fish_color_param
set --global fish_color_valid_path --underline
set --global fish_color_quote yellow
set --global fish_color_escape $fish_color_quote
set --global fish_color_operator cyan
set --global fish_color_end $fish_color_operator
set --global fish_color_redirection $fish_color_operator
set --global fish_color_error red
set --global fish_color_cancel brblack
set --global fish_color_comment brblack --italics
set --global fish_color_autosuggestion brblack
if test -e "$__fish_config_dir/conf.d/plugin-fish-colored-man.fish"
    set --global man_blink --reverse normal
    set --global man_bold --bold normal
    set --global man_standout normal
    set --global man_underline --underline normal
end

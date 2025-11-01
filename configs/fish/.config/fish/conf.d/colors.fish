set --global --export COLORTERM "$TERM"
set --global fish_color_command black
set --global fish_color_keyword $fish_color_command
set --global fish_color_param $fish_color_command
set --global fish_color_option $fish_color_param
set --global fish_color_valid_path --underline
set --global fish_color_quote yellow
set --global fish_color_escape $fish_color_quote
set --global fish_color_operator yellow
set --global fish_color_end $fish_color_operator
set --global fish_color_redirection $fish_color_operator
set --global fish_color_error red
set --global fish_color_cancel brblue
set --global fish_color_comment brblue --italics
set --global fish_color_autosuggestion brblue
if test -e "$__fish_config_dir/conf.d/plugin-fish-colored-man.fish"
    set --global man_blink --reverse black
    set --global man_bold --bold black
    set --global man_standout black
    set --global man_underline --underline black
end

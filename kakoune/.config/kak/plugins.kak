source ~/.config/kak/plugins/plug.kak/rc/plug.kak
plug 'andreyorst/plug.kak' 'noload'
plug 'alexherbo2/auto-pairs.kak' %{
  hook global WinCreate .* %{ auto-pairs-enable }
}
plug 'andreyorst/fzf.kak' %{
  set-option global fzf_file_command 'fd'
  map global user f ': fzf-mode<ret>' -docstring 'FZF mode'
}
plug 'Delapouite/kakoune-auto-percent'
plug 'evanrelf/kakoune-number-toggle' %{
  set-option global number_toggle_params -hlcursor -separator ' '
}

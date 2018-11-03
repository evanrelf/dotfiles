source ~/.config/kak/plugins/plug.kak/rc/plug.kak
plug 'andreyorst/plug.kak' 'noload'

plug 'ul/kak-lsp' %{
  map global user 'l' ': enter-user-mode lsp<ret>' -docstring 'LSP mode'
}
plug 'andreyorst/fzf.kak' %{
  set-option global fzf_file_command 'fd'
  map global user 'f' ': fzf-mode<ret>' -docstring 'FZF mode'
}
plug 'alexherbo2/auto-pairs.kak' %{
  hook global WinCreate .* %{ auto-pairs-enable }
}
plug 'h-youhei/kakoune-surround' %{
  declare-user-mode surround
  map global surround 'a' ': surround<ret>' -docstring 'Add'
  map global surround 'r' ': change-surround<ret>' -docstring 'Change'
  map global surround 'd' ': delete-surround<ret>' -docstring 'Delete'
  map global surround 't' ': select-surrounding-tag<ret>' -docstring 'Select tag'
  map global user 's' ': enter-user-mode surround<ret>' -docstring 'Surround mode'
}
plug 'Delapouite/kakoune-auto-percent'
plug 'Delapouite/kakoune-text-objects'
plug 'evanrelf/kakoune-number-toggle' %{
  set-option global number_toggle_params -hlcursor -separator ' '
}

source '~/.config/kak/plugins/plug.kak/rc/plug.kak'

plug 'andreyorst/plug.kak' noload

# plug 'andreyorst/powerline.kak' %{
#   hook -once global WinCreate .* %{
#     set-option global powerline_separator ''
#     set-option global powerline_separator_thin ''
#     powerline-theme solarized-dark
#     powerline-format git bufname line_column mode_info filetype
#   }
# }

plug 'ul/kak-lsp' do %{ cargo build --release } noload %{
  hook global WinSetOption filetype=(haskell|c|cpp) %{
    evaluate-commands %sh{ kak-lsp --kakoune -s $kak_session }
    map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP mode'
    set-option global lsp_hover_anchor true
    lsp-auto-hover-enable
  }
}

# plug 'andreyorst/fzf.kak' %{
#   set-option global fzf_file_command 'fd'
#   map global user 'f' ': fzf-mode<ret>' -docstring 'FZF mode'
# }

# plug 'alexherbo2/auto-pairs.kak' %{
#   set-option global auto_pairs ( ) { } [ ] '"' '"' "'" "'" ` `
#   hook global WinCreate .* auto-pairs-enable
# }

plug 'h-youhei/kakoune-surround' %{
  declare-user-mode surround
  map global surround a ': surround<ret>' -docstring 'Add'
  map global surround r ': change-surround<ret>' -docstring 'Change'
  map global surround d ': delete-surround<ret>' -docstring 'Delete'
  map global surround t ': select-surrounding-tag<ret>' -docstring 'Select tag'
  map global user s ': enter-user-mode surround<ret>' -docstring 'Surround mode'
}

plug 'Delapouite/kakoune-text-objects'

# plug 'andreyorst/smarttab.kak' %{
#   expandtab
#   set-option softtabstop 2
# }

plug 'Delapouite/kakoune-auto-percent'

plug 'Delapouite/kakoune-auto-star'

plug 'evanrelf/kakoune-number-toggle' %{
  set-option global number_toggle_params -hlcursor -separator ' '
}

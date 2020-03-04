provide-module "user_plugins" %{

source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

# NOTE: alexherbo2 frequently changes their plugins' APIs, so it's best to pin
# their plugins to specific commits for stability. I've had my Kakoune config
# break multiple times because of this...

# Toggle between relative and absolute line numbers depending on mode
# plug "evanrelf/number-toggle.kak" %{
#   set-option global number_toggle_params -hlcursor
# }

# Automatically complete pairs
plug "alexherbo2/auto-pairs.kak" \
commit "11fbd7c44f04c6092b5197419d669ec505a02f01" %{
  hook global WinCreate .* %{ auto-pairs-enable }
}

# Snippets
plug "alexherbo2/snippets.kak" \
commit "7616a810739590c03a216ad13e601ea923b1e552" %{
  hook global WinCreate .* %{ snippets-enable }
}

# Replace mode
plug "alexherbo2/replace-mode.kak" \
commit "a569d3df8311a0447e65348a7d48c2dea5415df0" %{
  map global normal "<a-r>" ": enter-replace-mode<ret><c-o>"
}

# Create parent directories if missing
plug "alexherbo2/mkdir.kak" \
commit "cb7848045390d7c4d7b729327971acd11d026866" %{
  hook global BufWritePre .* %{ mkdir-current-buffer }
}

# Move selections
plug "alexherbo2/move-line.kak" \
commit "00221c1ddb2d9ef984facfbdc71b56b789daddaf" %{
  map global normal "<up>" ": move-line-above<ret>"
  map global normal "<down>"  ": move-line-below<ret>"
}

# More text objects
plug "Delapouite/kakoune-text-objects" %{
  # Text object for paragraphs separated by two blank lines (like Elm)
  define-command -hidden text-object-spaced-paragraph %{
    execute-keys "<a-i>c\n\n\n,\n\n\n<ret>" %sh{
      [ "$kak_opt_objects_last_mode" = "<a-a>" ] && echo "JJ"
    }
  }
  map global object "<a-p>" "<esc>: text-object-spaced-paragraph<ret>" -docstring "spaced paragraph"
}

# Syntax highlighting for Graphviz
plug "jwhett/graphviz-kak"

# Manipulate buffers more quickly
plug "Delapouite/kakoune-buffers" %{
  map global normal "b" ": enter-user-mode buffers<ret>"
  map global normal "B" ": enter-user-mode -lock buffers<ret>"
}

# Change directory
plug "Delapouite/kakoune-cd" %{
  alias global cd change-directory-current-buffer
  alias global r change-directory-project-root
  alias global pwd print-working-directory
}

# FZF integration
plug "andreyorst/fzf.kak" %{
  map global user "f" ": fzf-mode<ret>" -docstring "FZF mode"
} defer "fzf" %{
  set-option global fzf_file_command "fd --type f --follow --hidden"
  set-option global fzf_preview false
  set-option global fzf_file_preview false
  set-option global fzf_vertical_map "ctrl-x"
  set-option global fzf_horizontal_map "ctrl-v"
}

# Surround selections with brackets
plug "h-youhei/kakoune-surround" %{
  map global user "s" ": enter-user-mode surround<ret>" -docstring "Surround mode"
  declare-user-mode "surround"
  map global surround "s" ": select-surround<ret>" -docstring "Select surround"
  map global surround "a" ": surround<ret>" -docstring "Add surround"
  map global surround "r" ": change-surround<ret>" -docstring "Replace surround"
  map global surround "d" ": delete-surround<ret>" -docstring "Delete surround"
  map global surround "t" ": enter-user-mode surround-tag<ret>" -docstring "Surround tag"
  declare-user-mode "surround-tag"
  map global surround-tag "t" ": select-surrounding-tag<ret>" -docstring "Select surrounding tag"
  map global surround-tag "a" ": surround-with-tag<ret>" -docstring "Add surrounding tag"
  map global surround-tag "r" ": change-surrounding-tag<ret>" -docstring "Replace surrounding tag"
  map global surround-tag "d" ": delete-surrounding-tag<ret>" -docstring "Delete surrounding tag"
}

# Language Server Protocol support
# plug "ul/kak-lsp" noload do %{
#   cargo install --locked --force --path .
# } %{
#   hook global WinSetOption filetype=(haskell) %{
#     eval %sh{kak-lsp --kakoune -s $kak_session --config ~/.config/kak-lsp/kak-lsp.toml}
#     set-option window lsp_diagnostic_line_error_sign "!"
#     set-option window lsp_diagnostic_line_warning_sign "?"
#     set-option window lsp_hover_anchor true
#     lsp-enable-window
#     map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
#   }
#   hook global KakEnd .* lsp-exit
#   # set-option global lsp_completion_trigger "execute-keys 'h<a-h><a-k>\S[^\s,=;*(){}\[\]]\z<ret>'"
# }

}

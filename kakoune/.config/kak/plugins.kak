source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

plug "evanrelf/primer.kak" theme

# Toggle between relative and absolute line numbers depending on mode
plug "evanrelf/number-toggle.kak" branch "override-highlighter" config %{
  set-option global number_toggle_params -hlcursor
}

# Snippets
plug "alexherbo2/snippets.kak" \
commit "7616a810739590c03a216ad13e601ea923b1e552" config %{
  hook global WinCreate .* %{ snippets-enable }
}

# Replace mode
plug "alexherbo2/replace-mode.kak" \
commit "a569d3df8311a0447e65348a7d48c2dea5415df0" config %{
  map global normal "<a-r>" ": enter-replace-mode<ret><c-o>"
}

# Syntax highlighting for Graphviz
plug "jwhett/graphviz-kak"

# Manipulate buffers more quickly
plug "Delapouite/kakoune-buffers" config %{
  map global normal "b" ": enter-user-mode buffers<ret>"
  map global normal "B" ": enter-user-mode -lock buffers<ret>"
}

# Change directory
plug "Delapouite/kakoune-cd" config %{
  alias global "cd" "change-directory-current-buffer"
  alias global "r" "change-directory-project-root"
  alias global "pwd" "print-working-directory"
}

# Create parent directories if missing
plug "alexherbo2/mkdir.kak" \
commit "cb7848045390d7c4d7b729327971acd11d026866" config %{
  hook global BufWritePre .* %{ mkdir-current-buffer }
}

# Surround selections with brackets
plug "h-youhei/kakoune-surround" config %{
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

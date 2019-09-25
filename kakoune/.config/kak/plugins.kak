source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

# Toggle between relative and absolute line numbers depending on mode
plug "evanrelf/number-toggle.kak" %{
  set-option global number_toggle_params -hlcursor -separator " "
}

# Temporary highlighting of search matches
plug "alexherbo2/search-highlighter.kak" %{
  hook global WinCreate .* %{ search-highlighter-enable }
}

# FZF integration
# plug "andreyorst/fzf.kak" %{
plug "evanrelf/fzf.kak" %{
  set-option global fzf_file_command "fd --type f --follow --hidden"
  set-option global fzf_preview false
}

# Surround selections with brackets
plug "h-youhei/kakoune-surround" %{
  map global user "s" ": enter-user-mode surround<ret>" -docstring "Surround mode"
  declare-user-mode "surround"
  map global "surround" "s" ": select-surround<ret>" -docstring "Select surround"
  map global "surround" "a" ": surround<ret>" -docstring "Add surround"
  map global "surround" "r" ": change-surround<ret>" -docstring "Replace surround"
  map global "surround" "d" ": delete-surround<ret>" -docstring "Delete surround"
  map global "surround" "t" ": enter-user-mode surround-tag<ret>" -docstring "Surround tag"
  declare-user-mode "surround-tag"
  map global "surround-tag" "t" ": select-surrounding-tag<ret>" -docstring "Select surrounding tag"
  map global "surround-tag" "a" ": surround-with-tag<ret>" -docstring "Add surrounding tag"
  map global "surround-tag" "r" ": change-surrounding-tag<ret>" -docstring "Replace surrounding tag"
  map global "surround-tag" "d" ": delete-surrounding-tag<ret>" -docstring "Delete surrounding tag"
}

# More text objects
plug "Delapouite/kakoune-text-objects"

# Automatically select entire buffer when appropriate
plug "Delapouite/kakoune-auto-percent"

# Automatically use word under cursor when advancing search without a selection
plug "Delapouite/kakoune-auto-star"

# Replace mode
plug "alexherbo2/replace.kak" %{
  map global user "r" ": replace<ret>" -docstring "Replace mode"
}

plug "alexherbo2/auto-pairs.kak" %{
  hook global WinCreate .* %{ auto-pairs-enable }
}

# Language Server Protocol support
plug "ul/kak-lsp" do %{
  cargo install --locked --force --path .
} %{
  # Enable LSP for certain filetypes
  hook global WinSetOption filetype=(haskell|purescript|rust|typescript|javascript) %{
    set-option global lsp_cmd "kak-lsp -s %val{session} -vvv --log /tmp/kak-lsp.log --config ~/.config/kak-lsp/kak-lsp.toml"
    lsp-enable-window
    # Show LSP info at cursor instead of at the bottom
    # set-option window lsp_hover_anchor true
    map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
  }
  # set-option global lsp_diagnostic_line_error_sign "!"
  # set-option global lsp_diagnostic_line_warning_sign "?"
  # set-option global lsp_completion_trigger "execute-keys 'h<a-h><a-k>\S[^\s,=;*(){}\[\]]\z<ret>'"
}

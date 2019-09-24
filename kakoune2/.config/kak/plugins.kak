source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

# Toggle between relative and absolute line numbers depending on mode
plug "evanrelf/kakoune-number-toggle" %{
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

# Language Server Protocol support
plug "ul/kak-lsp" do %{
  cargo build --release --locked
  cargo install --force --path .
} %{
  define-command lsp-restart %{ lsp-stop; lsp-start }
  set-option global lsp_completion_trigger "execute-keys 'h<a-h><a-k>\S[^\s,=;*(){}\[\]]\z<ret>'"
  set-option global lsp_diagnostic_line_error_sign "!"
  set-option global lsp_diagnostic_line_warning_sign "?"
  hook global WinSetOption filetype=(c|cpp|rust|haskell|purescript|javascript|typescript) %{
    map window user "l" ": enter-user-mode lsp<ret>" -docstring "LSP mode"
    lsp-enable-window
    set-option window lsp_hover_anchor true
    set-face window DiagnosticError default+u
    set-face window DiagnosticWarning default+u
  }
  hook global WinSetOption filetype=rust %{
    set-option window lsp_server_configuration rust.clippy_preference="on"
  }
  hook global KakEnd .* lsp-exit
}

source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "robertmeta/plug.kak" noload

plug "evanrelf/primer.kak" theme \
config %{
  colorscheme primer
  declare-option str column_color "rgb:f6f8fa"
}

plug "evanrelf/number-toggle.kak" \
config %{
  set-option global number_toggle_params -hlcursor
}

plug "evanrelf/byline.kak" \
config %{
  require-module "byline"
}

plug "alexherbo2/replace-mode.kak" \
commit "5f4c73cdbaf5aeb964ee35ad4b9081b233af90c0" \
config %{
  require-module "replace-mode"
  map global "normal" "<a-r>" ": enter-replace-mode<ret>"
}

# Dependency of `auto-pairs.kak`
plug "alexherbo2/prelude.kak" \
commit "f1e0f4d5cb62a36924e3f8ba6824d6aed8c19d23" \
config %{
  require-module "prelude"
}

plug "alexherbo2/auto-pairs.kak" \
commit "fd735ec149ef0d9ca5f628a95b1e52858b5afbdc" \
config %{
  require-module "auto-pairs"
  auto-pairs-enable
}

plug "h-youhei/kakoune-surround" \
commit "efe74c6f434d1e30eff70d4b0d737f55bf6c5022" \
config %{
  map global "user" "s" ": enter-user-mode surround<ret>" -docstring "Surround mode"
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

plug "listentolist/kakoune-fandt" \
commit "6b035782c2437708917ff1e4d3c05e33678e42dc" \
config %{
  require-module fandt
}

plug "andreyorst/fzf.kak" \
commit "f23daa698ad95493fbd675ae153e3cac13ef34e9" \
do %{
  # Remove lingering info popups
  git checkout .
  for file in rc/modules/*.kak; do sed -i "" "s/info -title/nop/g" "$file"; done
} \
config %{
  map global "user" "z" ": fzf-mode<ret>" -docstring "FZF..."
  map global "normal" "<a-o>" ": fzf-file<ret>"
} \
defer "fzf" %{
  set-option global fzf_file_command "fd --type f --follow --hidden --exclude .git"
  set-option global fzf_preview false
  set-option global fzf_file_preview false
  set-option global fzf_vertical_map "ctrl-x"
  set-option global fzf_horizontal_map "ctrl-v"
}

# plug "kak-lsp/kak-lsp" noload \
# commit "354b46e3cf56f0da35b444941a701ca4c1135aa8" \
# config %{
#   evaluate-commands %sh{kak-lsp --kakoune -s "$kak_session" --config=$HOME/.config/kak-lsp/kak-lsp.toml}
#   set-option global lsp_cmd "kak-lsp -s %val{session} -vvv --log /tmp/kak-lsp.log --config=$HOME/.config/kak-lsp/kak-lsp.toml"
#   hook global WinSetOption filetype=(haskell) %{
#     echo -debug "Enabling LSP for filetype %opt{filetype}"
#     lsp-enable-window
#   }
#   hook global KakEnd .* %{ lsp-exit }
# }

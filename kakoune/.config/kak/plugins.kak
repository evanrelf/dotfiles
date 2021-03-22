source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

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

plug "listentolist/kakoune-fandt" \
commit "6b035782c2437708917ff1e4d3c05e33678e42dc" \
config %{
  require-module fandt
}

# Dependency of `auto-pairs.kak`
plug "kakounedotcom/prelude.kak" \
commit "5dbdc020c546032885c1fdb463e366cc89fc15ad" \
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

# `alexherbo2` is a volatile plugin maintainer (random API changes with no
# backwards compatibility, unnecessary dependencies, etc.) so if I don't pin
# their plugins, things will break all the time.
#
# `auto-pairs.kak` should stay at `fd735ec` forever to avoid depending on
# `kakoune.cr`.

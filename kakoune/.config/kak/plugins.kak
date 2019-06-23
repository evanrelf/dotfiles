source "%val{config}/plugins/plug.kak/rc/plug.kak"

plug "andreyorst/plug.kak" noload

# Appearance
plug "evanrelf/kakoune-number-toggle" %{
  set-option global number_toggle_params -hlcursor -separator " "
}
plug "alexherbo2/search-highlighter.kak" %{
  hook global WinCreate .* %{
    search-highlighter-enable
  }
}

# Editing
plug "andreyorst/smarttab.kak" %{
  set-option gloabl expandtab
  set-option global softtabstop 2
  hook global WinSetOption filetype=(makefile) noexpandtab
  hook global WinSetOption filetype=(rust|python) softtabstop 4
}
plug "alexherbo2/auto-pairs.kak" %{
  hook global WinCreate .* %{
    auto-pairs-enable
    # auto-pairs-surround
  }
}
plug "h-youhei/kakoune-surround" %{
  map global user s ": enter-user-mode surround<ret>" -docstring "Surround mode"
  declare-user-mode "surround"
  map global "surround" s ": select-surround<ret>" -docstring "Select surround"
  map global "surround" a ": surround<ret>" -docstring "Add surround"
  map global "surround" r ": change-surround<ret>" -docstring "Replace surround"
  map global "surround" d ": delete-surround<ret>" -docstring "Delete surround"
  map global "surround" t ": enter-user-mode surround-tag<ret>" -docstring "Surround tag"
  declare-user-mode "surround-tag"
  map global "surround-tag" t ": select-surrounding-tag<ret>" -docstring "Select surrounding tag"
  map global "surround-tag" a ": surround-with-tag<ret>" -docstring "Add surrounding tag"
  map global "surround-tag" r ": change-surrounding-tag<ret>" -docstring "Replace surrounding tag"
  map global "surround-tag" d ": delete-surrounding-tag<ret>" -docstring "Delete surrounding tag"
}
plug "Delapouite/kakoune-text-objects"
plug "Delapouite/kakoune-auto-percent"
plug "Delapouite/kakoune-auto-star"

# Other
plug "Delapouite/kakoune-cd" %{
  alias global "cdb" "change-directory-current-buffer"
  alias global "cdr" "change-directory-project-root"
  alias global "pwd" "print-working-directory"
}

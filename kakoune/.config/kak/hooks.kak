provide-module "user_hooks" %{

# Display Git diff in gutter
hook global WinCreate .* %{ try %{
  git show-diff
}}
hook global BufWritePost .* %{ try %{
  git update-diff
}}

# Highlight trailing whitespace
hook global ModeChange push:.*:insert %{
  remove-highlighter window/trailing-whitespace
}
hook global ModeChange pop:insert:.* %{
  add-highlighter window/trailing-whitespace regex \h+$ 0:default,rgb:ff869a
}

# Use Tab key in autocomplete menu
hook global InsertCompletionShow .* %{ try %{
  execute-keys -draft "h<a-K>\h<ret>"
  map window insert "<tab>" "<c-n>"
  map window insert "<s-tab>" "<c-p>"
}}
hook global InsertCompletionHide .* %{
  unmap window insert "<tab>" "<c-n>"
  unmap window insert "<s-tab>" "<c-p>"
}

# Highlight current selection
# declare-option -hidden regex current_selection
# set-face global CurrentSelection default,rgb:4d4d4d
# hook global NormalIdle .* %{
#   evaluate-commands -draft %{ try %{
#     execute-keys <a-k>.{2,}<ret>
#     set-option buffer current_selection "(?i)\Q%val{selection}\E"
#   } catch %{
#     set-option buffer current_selection ""
#   }}
# }
# add-highlighter global/current_selection dynregex "%%opt{current_selection}" 0:CurrentSelection

}

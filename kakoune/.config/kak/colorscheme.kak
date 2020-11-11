try %{
  colorscheme primer

  add-highlighter global/80 column 81 default,rgb:f6f8fa

  hook global WinSetOption filetype=git-commit %{
    remove-highlighter global/80
    add-highlighter window/ column 51 default,rgb:f6f8fa
    add-highlighter window/ column 73 default,rgb:f6f8fa
  }

} catch %{ try %{ nop %sh{
  # Download if colorscheme not installed
  mkdir -p "$kak_config/colors"
  curl -L "https://raw.githubusercontent.com/evanrelf/primer.kak/master/colors/primer.kak" -o "$kak_config/colors/primer.kak"
}}}

define-command -docstring "softwrap-enable: enable soft wrapping" \
softwrap-enable %{
  add-highlighter window/softwrap wrap -indent -width %opt{autowrap_column}
}

define-command -docstring "softwrap-disable: disable soft wrapping" \
softwrap-disable %{
  remove-highlighter window/softwrap
}

# Show Git diff
hook global WinCreate .* %{
  hook -once window NormalIdle .* %{ try %{
    git show-diff
    # Update Git diff after saving
    hook window BufWritePost .* %{ try %{
      git update-diff
    }}
  }}
}

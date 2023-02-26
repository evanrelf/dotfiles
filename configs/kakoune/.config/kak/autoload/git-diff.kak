hook global WinCreate .* %{
  hook -once window NormalIdle .* %{ try %{
    git show-diff
    hook window BufWritePost .* %{ try %{
      git update-diff
    }}
  }}
}

hook global BufCreate (WORKSPACE|.*\.bazel)$ %{
  set buffer filetype python
}

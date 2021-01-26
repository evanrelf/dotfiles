hook global WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck --external-sources --check-sourced --enable all --format gcc"
  # set-option window lintcmd "shellcheck --format gcc"
  hook window BufWritePost .* %{ lint-buffer }
}

# Check for shebangs
hook global WinSetOption filetype= %{ try %{
  execute-keys -draft "x<a-k>#!(/bin/sh|/bin/bash|/usr/bin/env bash)<ret>"
  set-option window filetype sh
} catch %{ try %{
  execute-keys -draft "x<a-k>#!/usr/bin/env nix-shell<ret>"
  execute-keys -draft "/#!\s*nix-shell<ret>xs-i (bash|sh)<ret>"
  set-option window filetype sh
}}}

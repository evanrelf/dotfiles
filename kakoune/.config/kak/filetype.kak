# Haskell
define-command -hidden haskell-language-pragma -params 0 %{
  # Cache GHC language extensions for times when GHC isn't available (e.g. outside Nix shell)
  nop %sh{
    mkdir -p "$HOME/.local/share/kak"
    if command -v ghc >/dev/null 2>&1; then
      ghc --supported-extensions > "$HOME/.local/share/kak/ghc-language-extensions"
    fi
  }
  prompt -shell-script-candidates "cat $HOME/.local/share/kak/ghc-language-extensions" "extension: " %{
    execute-keys -draft "i{-# LANGUAGE %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}
define-command -hidden haskell-options-pragma -params 0 %{
  # Cache GHC options for times when GHC isn't available (e.g. outside Nix shell)
  nop %sh{
    mkdir -p "$HOME/.local/share/kak"
    if command -v ghc >/dev/null 2>&1; then
      ghc --show-options > "$HOME/.local/share/kak/ghc-options"
    fi
  }
  prompt -shell-script-candidates "cat $HOME/.local/share/kak/ghc-options" "option: " %{
    execute-keys -draft "i{-# OPTIONS_GHC %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}
hook global WinSetOption filetype=haskell %{
  map window user "1" ": set-option window filetype haskell<ret>" -docstring "Use old Haskell filetype"
  map window user "2" ": set-option window filetype haskell2<ret>" -docstring "Use new Haskell filetype"
}
hook global WinSetOption filetype=(haskell|haskell2) %{
  set-option window lintcmd "hlint"
  # hook window -group lint BufWritePost .* %{ lint }
  # lint-enable
  add-snippet window "forall" "∀"
  add-snippet window "lang" "<a-;>: haskell-language-pragma<ret>"
  add-snippet window "opt" "<a-;>: haskell-options-pragma<ret>"
  map window user "o" "|ormolu<ret>" -docstring "Format selection with ormolu"
}
hook global WinCreate .*\.hs-boot %{
  set-option window filetype haskell
}
hook global WinCreate .*\.hs %{
  hook -once window WinSetOption filetype=haskell %{
    try %{ set-option window filetype haskell2 }
  }
}

# PureScript
hook global WinCreate .*\.purs %{
  set-option window filetype haskell2
  add-snippet window "forall" "∀"
}
# hook global WinSetOption filetype=purescript %{
#   # Highlight function name in type signatures
#   add-highlighter shared/purescript/code/ regex ^\h*(?:(?:where|let)\h+)?([_a-z]['\w]*)\s+::\s 1:meta
#   # Replace 'forall' with '∀'
#   add-highlighter shared/purescript/code/ regex ∀ 0:keyword
#   add-snippet window "forall" "∀"
#   set-option window comment_line "--"
#   set-option window comment_block_begin "{-"
#   set-option window comment_block_end "-}"
# }

# Dhall
hook global WinSetOption filetype=(dhall|dhall2) %{
  set-option window formatcmd "dhall format"
  # hook window -group format BufWritePre .* %{ format-buffer }
}
hook global WinCreate .*\.dhall %{
  hook -once window WinSetOption filetype=dhall %{
    try %{ set-option window filetype dhall2 }
  }
}

# Rust
hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --emit stdout"
  hook window -group format BufWritePre .* %{ format-buffer }
}

# Shell
hook global WinSetOption filetype=sh %{
  set-option window lintcmd "shellcheck -f gcc"
  hook window -group lint BufWritePost .* %{ lint }
  lint-enable
}
hook global WinSetOption filetype= %{ try %{
  execute-keys -draft "x<a-k>#!(/bin/sh|/bin/bash|/usr/bin/env bash)<ret>"
  set-option window filetype sh
} catch %{ try %{
  execute-keys -draft "x<a-k>#!/usr/bin/env nix-shell<ret>"
  execute-keys -draft "/#!\s*nix-shell<ret>xs-i (bash|sh)<ret>"
  set-option window filetype sh
}}}

# Fish
hook global WinSetOption filetype=fish %{
  set-option window indentwidth 4
}

# Markdown
hook global WinSetOption filetype=markdown %{
  remove-hooks window markdown-indent
  add-highlighter shared/markdown/comment region -recurse <!-- <!-- --> fill comment
  set-option window comment_block_begin "<!-- "
  set-option window comment_block_end " -->"
}

# HTML
hook global WinSetOption filetype=html %{
  set-option window comment_block_begin "<!-- "
  set-option window comment_block_end " -->"
}

# CSS
hook global WinSetOption filetype=css %{
  set-option window comment_block_begin "/* "
  set-option window comment_block_end " */"
}

# Git
hook global WinSetOption filetype=git-commit %{
  remove-highlighter global/80
  add-highlighter window/ column 51 default,rgb:252438
  add-highlighter window/ column 73 default,rgb:252438
  add-snippet window "date" '<a-;>!date +%Y-%m-%d<ret><backspace>'
}
hook global WinCreate git-revise-todo %{
  set-option window filetype git-rebase
  strip-whitespace
}

# Docker
hook global WinCreate (Dockerfile.*|.*\.dockerfile) %{
  set-option window filetype dockerfile
  # set-option window lintcmd "hadolint"
  # hook window -group lint BufWritePost .* %{ lint }
  # lint-enable
}

# Makefile
hook global WinSetOption filetype=makefile %{
  set-option window aligntab true
  set-option window indentwidth 0
}

# Kakoune
hook global WinCreate .*\.kak %{
  add-highlighter shared/kakrc/code/ regex add-snippet 0:keyword
}
hook global BufCreate \*scratch\* %{
  execute-keys '%d'
}

# Emacs Lisp
hook global WinCreate .*\.el %{
  set-option window filetype lisp
}

# plist
hook global WinCreate .*\.plist %{
  set-option window filetype xml
}

# # Prettier
# evaluate-commands %sh{
#   if command -v prettier >/dev/null 2>&1; then
#     printf "%s" "
#     hook global WinSetOption filetype=markdown %{ set-option window formatcmd 'prettier --stdin --parser markdown' }
#     hook global WinSetOption filetype=json %{ set-option window formatcmd 'prettier --stdin --parser json' }
#     hook global WinSetOption filetype=yaml %{ set-option window formatcmd 'prettier --stdin --parser yaml' }
#     hook global WinSetOption filetype=javascript %{ set-option window formatcmd 'prettier --stdin --parser javascript' }
#     hook global WinSetOption filetype=typescript %{ set-option window formatcmd 'prettier --stdin --parser typescript' }
#     hook global WinSetOption filetype=css %{ set-option window formatcmd 'prettier --stdin --parser css' }
#     hook global WinSetOption filetype=scss %{ set-option window formatcmd 'prettier --stdin --parser scss' }
#     hook global WinSetOption filetype=less %{ set-option window formatcmd 'prettier --stdin --parser less' }
#     "
#   fi
# }

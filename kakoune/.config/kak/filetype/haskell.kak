# Better Haskell syntax
source "%val{config}/syntax/haskell2.kak"

declare-user-mode "haskell"
declare-user-mode "cabal"

# Set filetype to `haskell2` only once (allows switching back to `haskell`
# filetype)
hook global WinCreate .*\.hs %{
  hook -once window WinSetOption filetype=haskell %{
    set-option window filetype haskell2
  }
}

# Use Haskell syntax highlighting with `*.hs-boot` files
hook global WinCreate .*\.hs-boot %{
  set-option window filetype haskell
}

hook global WinSetOption filetype=(haskell|haskell2) %{
  set-option window formatcmd "fourmolu -o -XBangPatterns -o -XTypeApplications"
  map window "user" "," ": enter-user-mode haskell<ret>" -docstring "Haskell..."
  map window "haskell" "l" ": haskell-insert-language-pragma<ret>" -docstring "Insert LANGUAGE pragma"
  map window "haskell" "o" ": haskell-insert-options-pragma<ret>" -docstring "Insert OPTIONS_GHC pragma"
  map window "haskell" "f" "|fourmolu -o -XBangPatterns -o -XTypeApplications<ret>" -docstring "Format (fourmolu)"
  map window "haskell" "F" "|stylish-haskell<ret>" -docstring "Format (stylish-haskell)"
}

hook global WinSetOption filetype=cabal %{
  map window "user" "," ": enter-user-mode cabal<ret>" -docstring "Cabal..."
  map window "cabal" "l" ": haskell-insert-language-extension<ret>" -docstring "Insert language extension"
  map window "cabal" "o" ": haskell-insert-option<ret>" -docstring "Insert GHC option"
}

define-command -hidden haskell-cache-language-extensions %{
  nop %sh{
    file="$HOME/.cache/kak/ghc-language-extensions"
    mkdir -p "$HOME/.cache/kak"
    if command -v ghc >/dev/null 2>&1 && [ ! -f "$file" ]; then
      ghc --supported-extensions | grep --invert-match --extended-regexp "(^No|GeneralisedNewtypeDeriving|Rank2Types|AutoDeriveTypeable|TypeInType|NullaryTypeClasses)" > "$file"
    fi
  }
}

define-command -hidden haskell-cache-options %{
  nop %sh{
    file="$HOME/.cache/kak/ghc-options"
    mkdir -p "$HOME/.cache/kak"
    if command -v ghc >/dev/null 2>&1 && [ ! -f "$file" ]; then
      ghc --show-options | grep --invert-match --extended-regexp "(^-X|-Wwarn=|-Werror=|-Wno-error=)" > "$file"
    fi
  }
}

define-command -hidden haskell-insert-language-pragma %{
  haskell-cache-language-extensions
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-language-extensions" "extension: " %{
    execute-keys -draft "i{-# LANGUAGE %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-options-pragma %{
  haskell-cache-options
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-options" "option: " %{
    execute-keys -draft "i{-# OPTIONS_GHC %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-language-extension %{
  haskell-cache-language-extensions
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-language-extensions" "extension: " %{
    execute-keys -draft "i%val{text}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-option %{
  haskell-cache-options
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-options" "option: " %{
    execute-keys -draft "i%val{text}<esc>"
    execute-keys "<esc>"
  }
}

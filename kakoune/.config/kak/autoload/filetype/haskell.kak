declare-user-mode "haskell"

# Better Haskell syntax
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
  map window "haskell" "i" ": haskell-insert-module-import<ret>" -docstring "Insert module import"
  map window "haskell" "I" ": haskell-insert-qualified-module-import<ret>" -docstring "Insert qualified module import"
  map window "haskell" "f" "|fourmolu -o -XBangPatterns -o -XTypeApplications<ret>" -docstring "Format (fourmolu)"
  map window "haskell" "F" "|stylish-haskell<ret>" -docstring "Format (stylish-haskell)"
}

define-command -hidden haskell-cache-language-extensions %{
  nop %sh{
    file="${HOME}/.cache/kak/ghc-language-extensions"
    mkdir -p "${HOME}/.cache/kak"
    if command -v ghc >/dev/null 2>&1 && [ ! -f "${file}" ]; then
      ghc --supported-extensions > "${file}"
    fi
  }
}

define-command -hidden haskell-cache-options %{
  nop %sh{
    file="${HOME}/.cache/kak/ghc-options"
    mkdir -p "${HOME}/.cache/kak"
    if command -v ghc >/dev/null 2>&1 && [ ! -f "${file}" ]; then
      ghc --show-options > "${file}"
    fi
  }
}

define-command -hidden haskell-insert-language-pragma %{
  haskell-cache-language-extensions
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-language-extensions | grep --invert-match --extended-regexp '(GeneralisedNewtypeDeriving|Rank2Types|AutoDeriveTypeable|TypeInType|NullaryTypeClasses)'" "extension: " %{
    execute-keys -draft "i{-# LANGUAGE %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-options-pragma %{
  haskell-cache-options
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-options | grep --invert-match --extended-regexp '(^-X|-Wwarn=|-Werror=|-Wno-error=)'" "option: " %{
    execute-keys -draft "i{-# OPTIONS_GHC %val{text} #-}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-module-import %{
  prompt -shell-script-candidates "cat $HOME/.local/share/kak/haskell-module-names.csv | cut -d ',' -f 2" "module: " %{ evaluate-commands %sh{
    echo "execute-keys -draft 'iimport ${kak_text}<esc>'"
    echo "execute-keys '<esc>'"
  }}
}

define-command -hidden haskell-insert-qualified-module-import %{
  prompt -shell-script-candidates "cat $HOME/.local/share/kak/haskell-module-names.csv | cut -d ',' -f 2" "module (qualified): " %{ evaluate-commands %sh{
    alias=$(cat "$HOME/.local/share/kak/haskell-module-names.csv" | grep "${kak_text}" | head -n 1 | cut -d ',' -f 3)
    echo "execute-keys -draft 'iimport qualified ${kak_text} as ${alias}<esc>'"
    echo "execute-keys '<esc>'"
  }}
}

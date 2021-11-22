declare-user-mode "cabal"

# Better Cabal syntax
# Set filetype to `cabal2` only once (allows switching back to `cabal` filetype)
hook global WinCreate .*\.cabal %{
  hook -once window WinSetOption filetype=cabal %{
    set-option window filetype cabal2
  }
}

hook global WinSetOption filetype=(cabal|cabal2) %{
  map window "user" "," ": enter-user-mode cabal<ret>" -docstring "Cabal..."
  map window "cabal" "l" ": haskell-insert-language-extension<ret>" -docstring "Insert language extension"
  map window "cabal" "o" ": haskell-insert-option<ret>" -docstring "Insert GHC option"
}

define-command -hidden haskell-insert-language-extension %{
  haskell-cache-language-extensions
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-language-extensions | grep --invert-match --extended-regexp '(GeneralisedNewtypeDeriving|Rank2Types|AutoDeriveTypeable|TypeInType|NullaryTypeClasses)'" "extension: " %{
    execute-keys -draft "i%val{text}<esc>"
    execute-keys "<esc>"
  }
}

define-command -hidden haskell-insert-option %{
  haskell-cache-options
  prompt -shell-script-candidates "cat $HOME/.cache/kak/ghc-options | grep --invert-match --extended-regexp '(^-X|-Wwarn=|-Werror=|-Wno-error=)'" "option: " %{
    execute-keys -draft "i%val{text}<esc>"
    execute-keys "<esc>"
  }
}

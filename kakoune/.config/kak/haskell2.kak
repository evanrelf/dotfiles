# Resources:
# - Indentation: https://en.wikibooks.org/wiki/Haskell/Indentation

hook global BufCreate .*[.](hs2) %{
    set-option buffer filetype haskell2
}

hook global WinSetOption filetype=haskell2 %{
    require-module haskell2
    set-option buffer extra_word_chars '_' "'"
    hook -once -always window WinSetOption filetype=.* %{ remove-hooks window haskell2-.+ }
}

hook -group haskell2-highlight global WinSetOption filetype=haskell2 %{
    add-highlighter window/haskell2 ref haskell2
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/haskell2 }
}

provide-module haskell2 %[

# Operator characters: !#$%&*+./<=>?@\^|-~:

add-highlighter shared/haskell2 regions
add-highlighter shared/haskell2/code default-region group
add-highlighter shared/haskell2/string       region " "                                  fill string
add-highlighter shared/haskell2/quasiquote   region -recurse \{-# \{-#              -#\} fill comment
add-highlighter shared/haskell2/comment      region -recurse \{- \{-                -\}  fill comment
add-highlighter shared/haskell2/line_comment region --(?:[^!#$%&*+./<=>?@\\\^|-~:]) $    fill comment
add-highlighter shared/haskell2/pragma       region 

]

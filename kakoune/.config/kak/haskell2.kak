# Resources:
# - Indentation: https://en.wikibooks.org/wiki/Haskell/Indentation

# NOTE: Order matters!

hook -group haskell2-highlight global BufCreate .*[.](hs2) %{
    set-option buffer filetype haskell2
}

hook -group haskell2-highlight global WinSetOption filetype=haskell2 %{
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
add-highlighter shared/haskell2/string region \" (?<!\\)(\\\\)*" fill string
add-highlighter shared/haskell2/comment region -recurse \{- \{- -\} fill comment
add-highlighter shared/haskell2/line_comment region --(?:[^!#$%&*+./<=>?@\\\^|-~:]) $ fill comment
# TODO
add-highlighter shared/haskell2/pragma region '\{-#' '#-\}' fill comment
# TODO: Support qualified names (e.g. [Module.quoter|contents|])
add-highlighter shared/haskell2/quasiquote region \[\b[_a-z]['\w]*#?\| \|\] regex \[\b[_a-z]['\w]*#?\|(.*?)\|\] 1:string

# TODO: Prevent matching against this: f''c'
add-highlighter shared/haskell2/code/character regex \B'([^\\']|\\['\w\d\\])' 0:string

# TODO: Need to conditionally highlight allowed binding names: qualified, hiding, family, ccall, as, stock, anyclass, via
# TODO: Add non-operator operators (e.g. =, ::, NOT ->, =>, etc.)
add-highlighter shared/haskell2/code/keyword group
add-highlighter shared/haskell2/code/keyword/ regex \b(case|class|data|default|deriving|deriving|do|else|foreign|if|import|in|instance|mdo|module|newtype|of|pattern|proc|rec|then|type|where)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/ regex \bderiving\b\s+\b(stock|anyclass)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/ regex \bderiving\b\s+.+\s+\b(via)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/ regex \bimport\b.*?\b(qualified)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/ regex \bimport\b.*?\b(as)\b 1:keyword

# TODO: Needs more work (e.g. ignore when adjacent to square brackets)
add-highlighter shared/haskell2/code/operator group
add-highlighter shared/haskell2/code/operator/ regex (?<!['\[])([!#$%&\*\+\./<=>?@\\\^|\-~:]+)(?!['\]]) 1:operator
add-highlighter shared/haskell2/code/operator/ regex (=|::|\|) 0:default

add-highlighter shared/haskell2/code/type regex \b((?:[A-Z]\w*\.)*[A-Z_][\w']*) 1:type

add-highlighter shared/haskell2/code/module group
# TODO: PackageImports breaks this for some reason
# Module import
add-highlighter shared/haskell2/code/module/ regex \bimport\b\s.*?\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module
# Module import alias
add-highlighter shared/haskell2/code/module/ regex \bas\b\s\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module
# Module declaration
add-highlighter shared/haskell2/code/module/ regex \bmodule\b\s+\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module

]

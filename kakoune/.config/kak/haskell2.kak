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

  # TODO: Write your own indentation logic
  hook -group haskell2-trim-indent window ModeChange pop:insert:.* haskell2-trim-indent
  hook -group haskell2-indent window InsertChar \n haskell2-indent-on-new-line
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

# TODO: Needs more work (e.g. ignore when adjacent to square brackets, ignore backslash in \case, etc.)
add-highlighter shared/haskell2/code/operator regex (?<!['\[])([!#$%&\*\+\./<=>?@\\\^|\-~:]+)(?!['\]]) 1:operator

add-highlighter shared/haskell2/code/type regex \b((?:[A-Z]\w*\.)*(?:[A-Z]|_[A-Z])[\w']*) 1:type

add-highlighter shared/haskell2/code/module group
# TODO: PackageImports breaks this for some reason
# Module import
add-highlighter shared/haskell2/code/module/ regex \bimport\b\s.*?\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module
# Module import alias
add-highlighter shared/haskell2/code/module/ regex \bas\b\s\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module
# Module declaration
add-highlighter shared/haskell2/code/module/ regex \bmodule\b\s+\b((?:[A-Z]\w*)(?:\.[A-Z]\w*)*)\b 1:module


# TODO: Write your own indentation logic
define-command -hidden haskell2-trim-indent %{
  # remove trailing white spaces
  try %{ execute-keys -draft -itersel <a-x> s \h+$ <ret> d }
}

# TODO: Write your own indentation logic
define-command -hidden haskell2-indent-on-new-line %{
  evaluate-commands -draft -itersel %{
    # copy -- comments prefix and following white spaces
    try %{ execute-keys -draft k <a-x> s ^\h*\K--\h* <ret> y gh j P }
    # preserve previous line indent
    try %{ execute-keys -draft <semicolon> K <a-&> }
    # align to first clause
    try %{ execute-keys -draft <semicolon> k x X s ^\h*(if|then|else)?\h*(([\w']+\h+)+=)?\h*(case\h+[\w']+\h+of|do|let|where)\h+\K.* <ret> s \A|.\z <ret> & }
    # filter previous line
    try %{ execute-keys -draft k : haskell-trim-indent <ret> }
    # indent after lines beginning with condition or ending with expression or =(
    try %{ execute-keys -draft <semicolon> k x <a-k> ^\h*(if)|(case\h+[\w']+\h+of|do|let|where|[=(])$ <ret> j <a-gt> }
  }
}

]

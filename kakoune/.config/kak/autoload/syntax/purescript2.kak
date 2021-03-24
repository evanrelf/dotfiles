# Resources:
# - Indentation: https://en.wikibooks.org/wiki/Haskell/Indentation

# TODO: Top-level operator definitions aren't highlighted correctly (e.g. `f $ x`)
# TODO: Operators next to `[]` aren't highlighted correctly (e.g. `[+ 1]`)

hook -group purescript2-highlight global BufCreate .*[.](hs2) %{
  set-option buffer filetype purescript2
}

hook -group purescript2-highlight global WinSetOption filetype=purescript2 %{
  require-module purescript2

  set-option buffer extra_word_chars '_' "'"
  hook -once -always window WinSetOption filetype=.* %{ remove-hooks window purescript2-.+ }

  add-highlighter window/purescript2 ref purescript2
  hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/purescript2 }

  # TODO: Write your own indentation logic
  hook -group purescript2-trim-indent window ModeChange pop:insert:.* purescript2-trim-indent
  hook -group purescript2-indent window InsertChar \n purescript2-indent-on-new-line
}

provide-module purescript2 %§

add-highlighter shared/purescript2 regions
add-highlighter shared/purescript2/code default-region group
# TODO: This is broken: '"' (this still works: '\"')
add-highlighter shared/purescript2/string region '(?<![\\])"' (?<!\\)(\\\\)*" fill string
add-highlighter shared/purescript2/comment region -recurse \{-(?!#) \{-(?!#) (?<!#)-\} fill comment
add-highlighter shared/purescript2/line_comment region --(?:[^!#$%&*+./<=>?@\\\^|-~:]) $ fill comment
add-highlighter shared/purescript2/code/operator regex (?<![\[])('?(?:(?:[A-Z][\w']*\.)*)(?:[!#$%&\*\+\./<=>?@\\\^|\-~:]{2,}|[!#$%&\*\+/<>?\^\-:]|(?<![\w'])\.(?!\w)))(?!['\]]) 1:operator
add-highlighter shared/purescript2/code/top-level-binding regex ^(\w[\w']*)\s+ 1:function
add-highlighter shared/purescript2/code/keyword group
add-highlighter shared/purescript2/code/keyword/reserved-words regex (\\case\b|(?<!\.)\b(?:case|class|data|default|deriving|do|else|foreign|if|import|in|instance|let|mdo|module|newtype|of|pattern|proc|rec|then|type|where)\b) 1:keyword
add-highlighter shared/purescript2/code/keyword/forall regex (\bforall\b|∀)(?:\s+[a-z_][\w']*)+\s*(\.|->) 1:keyword 2:keyword
add-highlighter shared/purescript2/code/keyword/symbols regex (!(?=\w)|(?<![\w'])_(?![\w'])|[\{\}\(\)\[\],\;]|(?<![!#$%&\*\+\./<=>?@\\\^|\-~:'])(?:[=\|\\@~](?!')|=>|->|<-|::|\.\.)(?![!#$%&\*\+\./<=>?@\^|\-~:])) 1:keyword
add-highlighter shared/purescript2/code/keyword/promotion regex ('\[|'\(|@') 1:keyword
add-highlighter shared/purescript2/code/type regex (?<![\w'])('{0,2}(?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)(?![\.\w]) 1:type
add-highlighter shared/purescript2/code/infix regex `(?:(?:[A-Z][\w']*\.)*)\w[\w']*` 0:operator
add-highlighter shared/purescript2/code/module group
add-highlighter shared/purescript2/code/module/import regex (import)(?:\s+(qualified))?(?:\s+("[\w-]*?"))?\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*)(?:\s+(qualified))?(?:\s+(as)\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*))?(?:\s+(hiding)\s+\(.*?\))? 1:keyword 2:keyword 3:string 4:module 5:keyword 6:keyword 7:module 8:keyword
add-highlighter shared/purescript2/code/module/declaration regex \bmodule\b\s+\b((?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)\b 1:module
add-highlighter shared/purescript2/code/numbers group
add-highlighter shared/purescript2/code/numbers/decimal regex ((\b|-)[0-9](?:[0-9_]*[0-9])?(?:\.[0-9](?:[0-9_]*[0-9])?)?(?:[0-9_]*e[+-]?[0-9]+)?)\b 1:value
add-highlighter shared/purescript2/code/numbers/hexadecimal regex \b(0x[0-9a-f_]*[0-9a-f])\b 1:value
add-highlighter shared/purescript2/code/numbers/binary regex \b(0b[01_+]*[01])\b 1:value
add-highlighter shared/purescript2/code/character regex (?<!')\B'([^\\']|\\['"\w\d\\])' 0:string

# TODO: Write your own indentation logic
define-command -hidden purescript2-trim-indent %{
  # remove trailing white spaces
  try %{ execute-keys -draft -itersel <a-x> s \h+$ <ret> d }
}

# TODO: Write your own indentation logic
define-command -hidden purescript2-indent-on-new-line %{
  evaluate-commands -draft -itersel %{
    # copy -- comments prefix and following white spaces
    try %{ execute-keys -draft k <a-x> s ^\h*\K--\h* <ret> y gh j P }
    # preserve previous line indent
    try %{ execute-keys -draft <semicolon> K <a-&> }
    # align to first clause
    try %{ execute-keys -draft <semicolon> k x X s ^\h*(if|then|else)?\h*(([\w']+\h+)+=)?\h*(case\h+[\w']+\h+of|do|let|where)\h+\K.* <ret> s \A|.\z <ret> & }
    # filter previous line
    try %{ execute-keys -draft k : purescript2-trim-indent <ret> }
    # indent after lines beginning with condition or ending with expression or =(
    try %{ execute-keys -draft <semicolon> k x <a-k> ^\h*if|[=(]$|\b(case\h+[\w']+\h+of|do|let|where)$ <ret> j <a-gt> }
  }
}

§

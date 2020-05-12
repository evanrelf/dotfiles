# Resources:
# - Indentation: https://en.wikibooks.org/wiki/Haskell/Indentation

# TODO: infixr, infixl, etc. https://wiki.haskell.org/Keywords
# TODO: -XStandaloneDeriving via (via keyword is located differently)
# TODO: [1 .. 10] works, but [1..10] doesn't
# TODO: Compare against existing Haskell syntax highlighting, CORRECTED to use
# the same faces
# TODO: makeLenses is highlighted like a top-level declaration
# TODO: Labels should be highlighted differently
# TODO: Top-level operator definitions aren't highlighted correctly (e.g. `f $ x`)
# TODO: Promoted operators aren't highlighted as operators (e.g. x ': xs)
# TODO: Type annotations with promoted data constructors (e.g. @'Head)

hook -group haskell2-highlight global BufCreate .*[.](hs2) %{
  set-option buffer filetype haskell2
}

hook -group haskell2-highlight global WinSetOption filetype=haskell2 %{
  require-module haskell2

  set-option buffer extra_word_chars '_' "'"
  hook -once -always window WinSetOption filetype=.* %{ remove-hooks window haskell2-.+ }

  add-highlighter window/haskell2 ref haskell2
  hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/haskell2 }

  # TODO: Write your own indentation logic
  hook -group haskell2-trim-indent window ModeChange pop:insert:.* haskell2-trim-indent
  hook -group haskell2-indent window InsertChar \n haskell2-indent-on-new-line
}

provide-module haskell2 %§

add-highlighter shared/haskell2 regions
add-highlighter shared/haskell2/code default-region group
# TODO: This is broken: '"' (this still works: '\"')
# TODO: This is broken: "\""
add-highlighter shared/haskell2/string region '(?<![\\])"' (?<!\\)(\\\\)*" fill string
add-highlighter shared/haskell2/comment region -recurse \{-(?!#) \{-(?!#) (?<!#)-\} fill comment
add-highlighter shared/haskell2/line_comment region --(?:[^!#$%&*+./<=>?@\\\^|-~:]) $ fill comment
add-highlighter shared/haskell2/pragma region '\{-#' '#-\}' fill meta
add-highlighter shared/haskell2/quasiquote-texp region \[\|\| \|\|\] regex (\[\|\|)(.*?)(\|\|\]) 1:keyword 2:string 3:keyword
add-highlighter shared/haskell2/quasiquote-exp region \[\| \|\] regex (\[\|)(.*?)(\|\]) 1:keyword 2:string 3:keyword
add-highlighter shared/haskell2/quasiquote-user-defined region \[\b(?:(?:[A-Z][\w']*\.)*)[_a-z][\w']*#?\| \|\] regex (\[)\b(?:(?:[A-Z][\w']*\.)*)[_a-z][\w']*#?(\|)(.*?)(\|\]) 1:keyword 2:keyword 3:string 4:keyword
add-highlighter shared/haskell2/cpp-or-shebang region '^#' $ fill meta
add-highlighter shared/haskell2/code/operator regex (?<!['\[])((?:(?:[A-Z][\w']*\.)*)(?:[!#$%&\*\+\./<=>?@\\\^|\-~:]{2,}|[!#$%&\*\+/<>?\^\-:]|(?<![\w'])\.(?!\w)))(?!['\]]) 1:operator
add-highlighter shared/haskell2/code/top-level-binding regex ^(\w[\w']*)\s+ 1:function
add-highlighter shared/haskell2/code/keyword group
add-highlighter shared/haskell2/code/keyword/reserved-words regex (\\case\b|(?<!\.)\b(?:case|class|data|default|deriving|deriving|do|else|foreign|if|import|in|instance|let|mdo|module|newtype|of|pattern|proc|rec|then|type|where)\b) 1:keyword
add-highlighter shared/haskell2/code/keyword/deriving-strategies regex \bderiving\b\s+\b(stock|anyclass)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/deriving-via regex \bderiving\b\s+.+?\s+\b(via)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/family regex \b(?:type|data)\b\s+\b(family)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/forall regex (\bforall\b|∀)(?:\s+[a-z_][\w']*)+\s*(\.|->) 1:keyword 2:keyword
add-highlighter shared/haskell2/code/keyword/symbols regex (!(?=\w)|(?<![\w'])_(?![\w'])|[\{\}\(\)\[\],\;]|(?<![!#$%&\*\+\./<=>?@\\\^|\-~:'])(?:[=\|\\@~](?!')|=>|->|<-|::|\.\.)(?![!#$%&\*\+\./<=>?@\^|\-~:])) 1:keyword
add-highlighter shared/haskell2/code/type regex \b((?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)\b(?!\.) 1:type
# TODO: Need to disambiguate unit from importing only instances, or not exporting anything
# add-highlighter shared/haskell2/code/type-unit regex \(\) 0:type
add-highlighter shared/haskell2/code/infix regex `(?:(?:[A-Z][\w']*\.)*)\w[\w']*` 0:operator
add-highlighter shared/haskell2/code/module group
# TODO: -XPackageImports breaks this for some reason
add-highlighter shared/haskell2/code/module/import regex (import)(?:\s+(qualified))?(?:\s+("[\w-]*?"))?\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*)(?:\s+(qualified))?(?:\s+(hiding)\s+\(.*?\))?(?:\s+(as)\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*))? 1:keyword 2:keyword 3:string 4:module 5:keyword 6:keyword 7:keyword 8:module
add-highlighter shared/haskell2/code/module/declaration regex \bmodule\b\s+\b((?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)\b 1:module
add-highlighter shared/haskell2/code/numbers group
add-highlighter shared/haskell2/code/numbers/decimal regex (?<!\.)\b([0-9](?:[0-9_]*[0-9])?(?:\.[0-9](?:[0-9_]*[0-9])?)?(?:[0-9_]*e[+-][0-9]+)?)\b(?!\.) 1:value
add-highlighter shared/haskell2/code/numbers/hexadecimal regex \b(0x[0-9a-f_]*[0-9a-f])\b 1:value
add-highlighter shared/haskell2/code/numbers/binary regex \b(0b[01_+]*[01])\b 1:value
add-highlighter shared/haskell2/code/character regex (?<!')\B'([^\\']|\\['"\w\d\\])' 0:string

# TODO: -XDataKinds (e.g. 'Promoted, '[Foo, Bar, Baz], '(Promoted Arg)) (tick should be highlighted with type)
# TODO: -XTemplateHaskell splices (e.g. $(makeLenses ''MyType))
# TODO: -XForeignFunctionInterface keywords (e.g. foreign, ccall, prim, capi, interruptible, etc.)
# TODO: -XMagicHash / -XOverloadedLabels (#) (needs to highlight when against other stuff, like [#name :- primary])

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
    try %{ execute-keys -draft k : haskell2-trim-indent <ret> }
    # indent after lines beginning with condition or ending with expression or =(
    try %{ execute-keys -draft <semicolon> k x <a-k> ^\h*(if)|(case\h+[\w']+\h+of|do|let|where|[=(])$ <ret> j <a-gt> }
  }
}

§

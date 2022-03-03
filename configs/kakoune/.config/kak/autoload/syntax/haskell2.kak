# Resources:
# - Indentation: https://en.wikibooks.org/wiki/Haskell/Indentation

# TODO: infixr, infixl, etc. https://wiki.haskell.org/Keywords
# TODO: -XStandaloneDeriving via (via keyword is located differently)
# TODO: Compare against existing Haskell syntax highlighting, CORRECTED to use
# the same faces
# TODO: makeLenses is highlighted like a top-level declaration
# TODO: Labels should be highlighted differently

declare-option str-list haskell2_static_words \
  "module" "where" "import" "import qualified" "qualified" "as" "hiding" \
  \
  "data" "data instance" "newtype" "newtype instance" "type" "type instance" \
  "type family" "family" "instance" "class" "where" "pattern" "default" \
  "foreign" \
  \
  "deriving" "deriving stock" "deriving newtype" "deriving anyclass" "stock" \
  "newtype" "anyclass" "via" \
  \
  "let" "in" "where" \
  \
  "do" "mdo" "proc" "rec" "if" "then" "else" "case" "of"

declare-option completions haskell2_completions

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

  set-option window static_words %opt{haskell2_static_words}
}

provide-module haskell2 %§

add-highlighter shared/haskell2 regions
add-highlighter shared/haskell2/code default-region group
add-highlighter shared/haskell2/string region '(?<![\\])"' (?<!\\)(\\\\)*" fill string
# TODO: Doesn't highlight `'_'`, `'['`, `'('` correctly
add-highlighter shared/haskell2/character region (?<![\\\w@])'(?!['\w\[\(]) (?<!\\)(\\\\)*' fill string
add-highlighter shared/haskell2/comment region -recurse \{-(?!#) \{-(?!#) (?<!#)-\} fill comment
add-highlighter shared/haskell2/line_comment region -- $ fill comment
add-highlighter shared/haskell2/pragma region '\{-#' '#-\}' fill meta
add-highlighter shared/haskell2/quasiquote-texp region \[\|\| \|\|\] regex (\[\|\|)(.*?)(\|\|\]) 1:keyword 2:string 3:keyword
add-highlighter shared/haskell2/quasiquote-exp region \[\| \|\] regex (\[\|)(.*?)(\|\]) 1:keyword 2:string 3:keyword
add-highlighter shared/haskell2/quasiquote-user-defined region \[\b(?:(?:[A-Z][\w']*\.)*)[_a-z][\w']*#?\| \|\] regex (\[)\b(?:(?:[A-Z][\w']*\.)*)[_a-z][\w']*#?(\|)(.*?)(\|\]) 1:keyword 2:keyword 3:string 4:keyword
add-highlighter shared/haskell2/cpp-or-shebang region '^#' $ fill meta
add-highlighter shared/haskell2/code/operator regex ('?(?:(?:[A-Z][\w']*\.)*)(?:[!#$%&\*\+\./<=>?@\\\^|\-~:]{2,}|[!#$%&\*\+/<>?\^\-:]|(?<![\w'])\.(?!\w)))(?![']) 1:operator
add-highlighter shared/haskell2/code/top-level-binding regex ^(\w[\w']*)#?\s+ 1:function
add-highlighter shared/haskell2/code/top-level-operator regex ^\((?<![\[])('?(?:(?:[A-Z][\w']*\.)*)(?:[!#$%&\*\+\./<=>?@\\\^|\-~:]{2,}|[!#$%&\*\+/<>?\^\-:]|(?<![\w'])\.(?!\w)))(?!['\]])\)\s+ 1:function
add-highlighter shared/haskell2/code/keyword group
add-highlighter shared/haskell2/code/keyword/reserved-words regex (\\case\b|(?<!\.)\b(?:case|class|data|default|deriving|do|else|foreign|if|import|in|instance|let|mdo|module|newtype|of|pattern|proc|rec|then|type|where)\b) 1:keyword
add-highlighter shared/haskell2/code/keyword/deriving-strategies regex \bderiving\b\s+\b(stock|anyclass|via)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/deriving-via regex \bderiving\b\s+.+?\s+\b(via)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/family regex \b(?:type|data)\b\s+\b(family)\b 1:keyword
add-highlighter shared/haskell2/code/keyword/forall regex (\bforall\b|∀)(?:\s+[a-z_][\w']*)+\s*(\.|->) 1:keyword 2:keyword
add-highlighter shared/haskell2/code/keyword/symbols regex (!(?=\w)|(?<![\w'])_(?![\w'])|[\{\}\(\)\[\],\;]|(?<![!#$%&\*\+\./<=>?@\\\^|\-~:'])(?:[=\|\\@~](?!')|=>|->|<-|-<|-<<|::|\.\.)(?![!#$%&\*\+\./<=>?@\^|\-~:])) 1:keyword
add-highlighter shared/haskell2/code/keyword/promotion regex ('\[|'\(|@') 1:keyword
add-highlighter shared/haskell2/code/type regex (?<![\w'])('{0,2}(?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)(?![\.\w]) 1:type
add-highlighter shared/haskell2/code/type-unit regex \'?\(\) 0:type
add-highlighter shared/haskell2/code/infix regex `(?:(?:[A-Z][\w']*\.)*)\w[\w']*` 0:operator
add-highlighter shared/haskell2/code/module group
# TODO: -XPackageImports breaks this for some reason
add-highlighter shared/haskell2/code/module/import regex (import)(?:\s+(qualified))?(?:\s+("[\w-]*?"))?\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*)(?:\s+(qualified))?(?:\s+(as)\s+([A-Z][\w']*(?:\.[A-Z][\w']*)*))?(?:\s+(hiding))?(?:\s+(\().*?(\)))? 1:keyword 2:keyword 3:string 4:module 5:keyword 6:keyword 7:module 8:keyword 9:keyword 10:keyword
add-highlighter shared/haskell2/code/module/declaration regex (module)\s+\b((?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)(?:\s+(\().*?(\)))?(?:\s+(where))\b 1:keyword 2:module 3:keyword 4:keyword 5:keyword
add-highlighter shared/haskell2/code/numbers group
add-highlighter shared/haskell2/code/numbers/decimal regex ((\b|-)[0-9](?:[0-9_]*[0-9])?(?:\.[0-9](?:[0-9_]*[0-9])?)?(?:[0-9_]*e[+-]?[0-9]+)?)\b 1:value
add-highlighter shared/haskell2/code/numbers/hexadecimal regex \b(0x[0-9a-f_]*[0-9a-f])\b 1:value
add-highlighter shared/haskell2/code/numbers/binary regex \b(0b[01_+]*[01])\b 1:value

# TODO: -XTemplateHaskell splices (e.g. $(makeLenses ''MyType))
# TODO: -XForeignFunctionInterface keywords (e.g. foreign, ccall, prim, capi, interruptible, etc.)
# TODO: -XMagicHash / -XOverloadedLabels (#)

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
    try %{ execute-keys -draft <semicolon> k x <a-k> ^\h*if|[=(]$|\b(case\h+[\w']+\h+of|do|let|where)$ <ret> j <a-gt> }
  }
}

§

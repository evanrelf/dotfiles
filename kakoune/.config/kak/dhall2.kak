hook -group dhall2-highlight global BufCreate .*[.](dhall2) %{
  set-option buffer filetype dhall2
}

hook -group dhall2-highlight global WinSetOption filetype=dhall2 %{
  require-module dhall2
  hook -once -always window WinSetOption filetype=.* %{ remove-hooks window dhall2-.+ }

  add-highlighter window/dhall2 ref dhall2
  hook -once -always window WinSetOption filetype=.* %{ remove-hooks window window/dhall2 }

  # TODO: Write your own indentation logic
  hook -group dhall2-trim-indent window ModeChange pop:insert:.* dhall2-trim-indent
  hook -group dhall2-indent window InsertChar \n dhall2-indent-on-new-line
}

provide-module dhall2 %§

add-highlighter shared/dhall2 regions
add-highlighter shared/dhall2/code default-region group
add-highlighter shared/dhall2/text region '(?<![\\])"' (?<!\\)(\\\\)*" fill string
add-highlighter shared/dhall2/multiline_text region "''" "''" fill string
add-highlighter shared/dhall2/comment region --(?:[^!#$%&*+./<=>?@\\\^|-~:]) $ fill comment
add-highlighter shared/dhall2/multiline_comment region -recurse \{-(?!#) \{-(?!#) (?<!#)-\} fill comment
add-highlighter shared/dhall2/code/keyword_words regex (:?\b(let|in|with|as|if|then|else|forall|assert)\b) 1:keyword
add-highlighter shared/dhall2/code/keyword_symbols regex ([\{\}\(\)\[\],:`=\|<>^\?\\λ∀→⫽≡]|->|//|::) 1:keyword
add-highlighter shared/dhall2/code/type regex \b((?:[A-Z][\w']*)(?:\.[A-Z][\w']*)*)\b(?!\.) 1:type
add-highlighter shared/dhall2/code/environment_variable regex \b(env:)\b(\w+)\b 1:keyword 2:string
add-highlighter shared/dhall2/code/semantic_hash regex \b(sha256:)\b([0-9a-fA-F]+)\b 1:keyword 2:string
add-highlighter shared/dhall2/code/numbers regex (?<!\.)([+-]?[0-9]+(?:\.[0-9]+)?(?:[0-9]*e[+-]?[0-9]+)?)\b(?!\.) 1:value

# TODO: Write your own indentation logic
define-command -hidden dhall2-trim-indent %{
  # remove trailing white spaces
  try %{ execute-keys -draft -itersel <a-x> s \h+$ <ret> d }
}

# TODO: Write your own indentation logic
define-command -hidden dhall2-indent-on-new-line %{
  evaluate-commands -draft -itersel %{
    # copy -- comments prefix and following white spaces
    try %{ execute-keys -draft k <a-x> s ^\h*\K--\h* <ret> y gh j P }
    # preserve previous line indent
    try %{ execute-keys -draft <semicolon> K <a-&> }
    # align to first clause
    try %{ execute-keys -draft <semicolon> k x X s ^\h*(if|then|else)?\h*(([\w']+\h+)+=)?\h*(let|in)\h+\K.* <ret> s \A|.\z <ret> & }
    # filter previous line
    try %{ execute-keys -draft k : dhall2-trim-indent <ret> }
    # indent after lines beginning with condition or ending with expression or =(
    try %{ execute-keys -draft <semicolon> k x <a-k> ^\h*(if)|(let|in|[=(])$ <ret> j <a-gt> }
  }
}

§

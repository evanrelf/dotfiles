# http://haskell.org/cabal2
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*[.](cabal) %{
    set-option buffer filetype cabal2
}

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook global WinSetOption filetype=cabal2 %[
    require-module cabal2

    hook window ModeChange pop:insert:.* -group cabal2-trim-indent  cabal2-trim-indent
    hook window InsertChar \n -group cabal2-insert cabal2-insert-on-new-line
    hook window InsertChar \n -group cabal2-indent cabal2-indent-on-new-line
    hook window InsertChar \{ -group cabal2-indent cabal2-indent-on-opening-curly-brace
    hook window InsertChar \} -group cabal2-indent cabal2-indent-on-closing-curly-brace

    hook -once -always window WinSetOption filetype=.* %{ remove-hooks window cabal2-.+ }
]

hook -group cabal2-highlight global WinSetOption filetype=cabal2 %{
    add-highlighter window/cabal2 ref cabal2
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/cabal2 }
}


provide-module cabal2 %[

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾

add-highlighter shared/cabal2 regions
add-highlighter shared/cabal2/code default-region group
add-highlighter shared/cabal2/line_comment region (--) $ fill comment
add-highlighter shared/cabal2/comment region -recurse \{- \{- -\} fill comment

add-highlighter shared/cabal2/code/ regex \b(true|false)\b|(([<>]?=?)?\d+(\.\d+)+) 0:value
add-highlighter shared/cabal2/code/ regex \b(if|else)\b 0:keyword
add-highlighter shared/cabal2/code/ regex ^\h*([A-Za-z][A-Za-z0-9_-]*)\h*: 1:variable

# Commands
# ‾‾‾‾‾‾‾‾

define-command -hidden cabal2-trim-indent %{
    # remove trailing white spaces
    try %{ execute-keys -draft -itersel <a-x> s \h+$ <ret> d }
}

define-command -hidden cabal2-insert-on-new-line %[
    evaluate-commands -draft -itersel %[
        # copy '--' comment prefix and following white spaces
        try %[ execute-keys -draft k <a-x> s ^\h*\K--\h* <ret> y gh j P ]
    ]
]

define-command -hidden cabal2-indent-on-new-line %[
    evaluate-commands -draft -itersel %[
        # preserve previous line indent
        try %[ execute-keys -draft <semicolon> K <a-&> ]
        # filter previous line
        try %[ execute-keys -draft k : cabal2-trim-indent <ret> ]
        # indent after lines ending with { or :
        try %[ execute-keys -draft <space> k <a-x> <a-k> [:{]$ <ret> j <a-gt> ]
        # deindent closing brace when after cursor
        try %[ execute-keys -draft <a-x> <a-k> \h*\} <ret> gh / \} <ret> m <a-S> 1<a-&> ]
    ]
]

define-command -hidden cabal2-indent-on-opening-curly-brace %[
    evaluate-commands -draft -itersel %[
        # align indent with opening paren when { is entered on a new line after the closing paren
        try %[ execute-keys -draft h <a-F> ) M <a-k> \A\(.*\)\h*\n\h*\{\z <ret> s \A|.\z <ret> 1<a-&> ]
    ]
]

define-command -hidden cabal2-indent-on-closing-curly-brace %[
    evaluate-commands -draft -itersel %[
        # align to opening curly brace when alone on a line
        try %[ execute-keys -draft <a-h> <a-k> ^\h+\}$ <ret> h m s \A|.\z<ret> 1<a-&> ]
    ]
]

]

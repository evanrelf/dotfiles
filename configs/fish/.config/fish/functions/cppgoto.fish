# Regular expressions copied from `jacktasia/dumb-jump` Emacs plugin
function cppgoto --wraps rg
    set --local match (\
        rg \
            --type 'c' \
            --type 'cpp' \
            --pcre2 \
            --regexp "\b(?<function1>$argv[1])(\s|\))*\((\w|[,&*.<>:]|\s)*(\))\s*(const|->|\{|\$)|typedef\s+(\w|[(*]|\s)+(?<function2>$argv[1])(\)|\s)*\(" \
            --regexp "\b(?!(class\b|struct\b|return\b|else\b|delete\b))(\w+|[,>])([*&]|\s)+(?<variable1>$argv[1])\s*(\[(\d|\s)*\])*\s*([=,(){;]|:\s*\d)|#define\s+(?<variable2>$argv[1])\b" \
            --regexp "\b(class|struct|enum|union)\b\s*(?<type1>$argv[1])\b\s*(final\s*)?(:((\s*\w+\s*::)*\s*\w*\s*<?(\s*\w+\s*::)*\w+>?\s*,*)+)?((\{|\$))|}\s*(?<type2>$argv[1])\b\s*;" \
            --replace '$function1$function2$variable1$variable2$type1$type2' \
            --only-matching \
            --column \
            --color 'always' \
            --colors 'path:none' \
            --colors 'line:none' \
            --colors 'column:none' \
            --colors 'match:fg:red' \
            --line-buffered \
            $argv[2..-1] \
        | fzf \
            (string split " " -- $FZF_DEFAULT_OPTS) \
            --scheme 'path' \
            --ansi \
            --select-1 \
            --exit-0 \
            --delimiter ':' \
            --with-nth '1,2,4' \
            --nth '1,3' \
            --with-shell 'fish -c' \
            --preview '
                set -l file {1}
                set -l line {2}
                set -l start_line $(math "max(0, $line - 5)")
                bat \
                    --number \
                    --line-range "$start_line:+$FZF_PREVIEW_LINES" \
                    --highlight-line "$line" \
                    --theme GitHub \
                    --color always \
                    "$file"
            ' \
            --preview-window 'down,75%' \
            --height '80%' \
        | cut -d ':' -f 1-3
    )
    if test -n "$match"
        echo "$match"
        "$EDITOR" "$match"
    end
end

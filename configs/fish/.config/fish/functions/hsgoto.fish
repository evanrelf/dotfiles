function hsgoto
    set --local match (\
        rg \
            --type haskell \
            --multiline \
            "^ *\b(?<term>$argv[1])\b\s+::|^ *\b(?:type|type\s+family|newtype|data|class)\b[^=]*? +\b(?<type>$argv[1])\b" \
            --replace '$term$type' \
            --only-matching \
            --column \
            --color always \
            --colors 'path:none' \
            --colors 'line:none' \
            --colors 'column:none' \
            --colors 'match:fg:red' \
            --line-buffered \
            $argv[2..-1] \
        | fzf \
            (string split " " -- $FZF_DEFAULT_OPTS) \
            --ansi \
            --exit-0 \
            --select-1 \
            --preview '
                set -l file (echo {} | cut -d ":" -f 1)
                set -l line (echo {} | cut -d ":" -f 2)
                awk "
                    NR >= $line {
                        print;
                        count++;
                        if (count == $FZF_PREVIEW_LINES) exit;
                    }
                " $file
            ' \
        | cut -d ':' -f 1-3
    )
    test -n "$match" && "$EDITOR" "$match"
end

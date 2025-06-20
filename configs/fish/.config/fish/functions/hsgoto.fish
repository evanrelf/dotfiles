function hsgoto
    set --local match (\
        rg \
            --type 'haskell' \
            --multiline \
            "^ *\b(?<term>$argv[1])\b\s+::|^ *\b(?:type [^=]*|type\s+family [^=]*?|newtype [^=]*?|data [^=]*?|class (?:.*? =>)?) *\b(?<type>$argv[1])\b" \
            --replace '$term$type' \
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

function hsgoto
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?<term>$argv[1])\b\s+::|^ *\b(?:type|type\s+family|newtype|data|class)\b.*? +\b(?<type>$argv[1])\b" \
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
        --delimiter ':' \
        --with-nth '1,2,4' \
        --nth '1,3' \
        --preview '
            set -l file {1}
            set -l line {2}
            awk "
                NR >= $line {
                    print;
                    count++;
                    if (count == $FZF_PREVIEW_LINES) exit;
                }
            " $file
        ' \
        --bind 'one,enter:become(echo {1..3}; $EDITOR {1..3})'
end

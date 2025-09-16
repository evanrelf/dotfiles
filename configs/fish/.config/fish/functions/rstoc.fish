function rstoc --wraps rstoc
    set --local match (\
        if test -z "$argv"; \
            command rstoc (fd -e rs); \
        else; \
            command rstoc $argv; \
        end \
        | fzf \
            (string split " " -- $FZF_DEFAULT_OPTS) \
            --scheme 'path' \
            --ansi \
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

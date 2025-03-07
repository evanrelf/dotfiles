function rstoc
    set --local match (\
        if test -z "$argv"; \
            command rstoc (fd -e rs); \
        else; \
            command rstoc $argv; \
        end \
        | fzf \
            (string split " " -- $FZF_DEFAULT_OPTS) \
            --ansi \
            --exit-0 \
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
        | cut -d ':' -f 1-2
    )
    test -n "$match" && "$EDITOR" "$match"
end

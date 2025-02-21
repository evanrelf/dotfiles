function rsgoto
    set --local fzf_opts
    # TODO: This sucks
    for opt in (echo "$FZF_DEFAULT_OPTS" | tr ' ' '\n')
        set --append fzf_opts $opt
    end
    set --local match (\
        rg \
            --type rust \
            --multiline \
            "\b(?:fn|static|const|let(?:\s+\bmut\b)?)\s+\b(?:$argv[1])\b|\b(?:type|struct|enum|union|trait(?:<.+>\s+)?)\s+\b(?:$argv[1])\b" \
            --line-number \
            $argv[2..-1] \
        | cut \
            -d ':' \
            -f 1-2 \
        | fzf $fzf_opts \
            --exit-0 \
            --select-1 \
            --preview '
                set -l file (echo {} | cut -d ":" -f 1)
                set -l line (echo {} | cut -d ":" -f 2)
                awk "
                    # NR >= $line && /^\s/ {
                    NR >= $line {
                        print;
                        count++;
                        if (count == $FZF_PREVIEW_LINES) exit;
                    }
                    # NR >= $line && !/^\s/ {
                    #     exit;
                    # }
                " $file
            ' \
    )
    test -n "$match" && "$EDITOR" "$match"
end

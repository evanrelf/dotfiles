function rsgoto
    set --local matches (mktemp)

    gum spin --title "Searching..." -- \
        rg \
        --type rust \
        --multiline \
        "\b(?:fn|static|const|let(?:\s+\bmut\b)?)\s+\b(?:$argv[1])\b|\b(?:type|struct|enum|union|trait(?:<.+>\s+)?)\s+\b(?:$argv[1])\b" \
        --line-number \
        $argv[2..-1] >"$matches"

    set --local count (wc -l < "$matches")

    if test "$count" -eq 0
        return 1
    else
        set --local match (cat "$matches" | eval "fzf $FZF_DEFAULT_OPTS -1" | cut -d ':' -f 1-2)
        if test -n "$match"
            "$EDITOR" "$match"
        end
    end
end

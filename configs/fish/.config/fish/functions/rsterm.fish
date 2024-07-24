function rsterm
    rg \
        --type rust \
        --multiline \
        "\b(?:fn|static|const|let(?:\s+\bmut\b)?)\s+\b(?:$argv[1])\b" \
        $argv[2..-1]
end

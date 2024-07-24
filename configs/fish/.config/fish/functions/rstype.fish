function rstype
    rg \
        --type rust \
        --multiline \
        "\b(?:type|struct|enum|union|trait(?:<.+>\s+)?)\s+\b(?:$argv[1])\b" \
        $argv[2..-1]
end

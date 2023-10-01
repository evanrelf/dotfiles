function hstype
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?:type|type\s+family|newtype|data|class)\b.*\s+\b(?:$argv[1])\b" \
        $argv[2..-1]
end

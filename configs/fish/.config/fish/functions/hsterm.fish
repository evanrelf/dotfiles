function hsterm
    rg \
        --type haskell \
        --multiline \
        "^ *\b(?:$argv[1])\b\s+::" \
        $argv[2..-1]
end

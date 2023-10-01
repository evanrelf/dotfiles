function hsimport
    rg \
        --type haskell \
        "^import +(?:\"[\w-]+\" +)?(?:qualified +)?\b$argv[1]\b(?: .*)?\$" \
        $argv[2..-1]
end

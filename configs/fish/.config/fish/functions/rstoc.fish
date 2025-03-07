# Only lists top-level items (i.e. not indented). That excludes declarations in
# `impl` blocks, `trait` blocks, `mod` blocks, declarative macros, etc.
function rstoc
    if test -n "$argv[1]"
        # If ripgrep is only given a single file to search as an argument, it
        # elides its path. I couldn't find an option to always print paths, so
        # giving it a second file that's always empty works as a hacky
        # alternative.
        set --append argv /dev/null
    end
    set --local match (\
        rg \
            --type rust \
            '^(?:pub(?:\((?:self|super|crate)\))?\s+)?\b(fn|type|enum|union|struct|trait|const|static)\s+\b(\w+)\b' \
            --replace '$1 $2' \
            --only-matching \
            --line-number \
            --color always \
            --colors 'path:none' \
            --colors 'line:none' \
            --colors 'column:none' \
            --colors 'match:fg:red' \
            --line-buffered \
            $argv \
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

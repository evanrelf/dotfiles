function edit-matches -d "Edit files containing a matching regular expression"
    for file in (rg --files-with-matches $argv)
        if echo "$EDITOR" | rg --quiet "kak"
            set --local position (rg --line-number --column --max-count 1 $argv -- $file | cut -d ':' -f 1-2)
            eval "$EDITOR $file +$position" || break
        else if echo "$EDITOR" | rg --quiet "(vim|nvim)"
            set --local line (rg --line-number --max-count 1 $argv -- $file | cut -d ':' -f 1)
            eval "$EDITOR $file +$line" || break
        else
            eval "$EDITOR $file" || break
        end
    end
end

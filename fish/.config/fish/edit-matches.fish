function edit-matches -d "Edit files containing a matching regular expression"
    for file in (rg --files-with-matches $argv)
        eval "$EDITOR $file" || break
    end
end

function __edit_file_position
    if test -z "$argv"
        return 1
    end

    _log "$argv"

    set --local file (echo "$argv" | cut -d ':' -f 1)
    set --local line (echo "$argv" | cut -d ':' -f 2)
    set --local column (echo "$argv" | cut -d ':' -f 3)

    switch "$EDITOR"
        case "kak"
            eval "$EDITOR $file +$line:$column"
        case "vi" "vim" "nvim"
            eval "$EDITOR $file '+normal $line''G$column|zz'"
        case "*"
            eval "$EDITOR $file"
    end
end

function rge -d "Edit file containing a matching regular expression"
    set --local rg_cmd "rg --line-number --column --with-filename --color always $argv"
    set --local fzf_cmd "fzf --exact -0"
    set --local match (eval "$rg_cmd | $fzf_cmd")
    __edit_file_position "$match" && rge $argv
end

function rges -d "Edit files containing a matching regular expression"
    for match in (rg --line-number --column --with-filename --max-count 1 $argv)
        __edit_file_position "$match" || return 1
    end
end

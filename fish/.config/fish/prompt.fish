set --global fish_greeting

function fish_prompt
    set --local exit_code "$status"
    set_color "$fish_color_cwd"
    echo -n (prompt_pwd)" "
    if _exists git
        git_prompt_segment
    end
    set_color normal
    if test "$exit_code" -ne 0
        set_color "$fish_color_error"
    end
    if test -n "$IN_NIX_SHELL"
        echo -n "* "
    else
        echo -n "λ "
    end
    set_color normal
end

function git_prompt_segment
    if _silently git rev-parse --git-dir
        if set --local branch (git symbolic-ref --short HEAD --quiet || git branch | head -n 1 | awk '{print $NF}' | tr -d ')')
            set --local dirty (git status --porcelain)
            if test -z "$dirty"
                # Clean
                set_color green
            else if echo "$dirty" | grep --invert-match --quiet "\?\?"
                # Dirty
                set_color yellow
            else
                # Dirty with untracked files
                set_color red
            end
            set --local branch_truncated (echo "$branch" | head -c 35)
            if test "$branch" != "$branch_truncated"
                echo -n "$branch_truncated… "
            else
                echo -n "$branch "
            end
            set_color normal
        end
    end
end

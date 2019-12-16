# Prompt
set -g fish_greeting ""
set -g fish_prompt_pwd_dir_length 0
set __fish_git_prompt_showdirtystate "true"
set __fish_git_prompt_showuntrackedfiles "true"
set __fish_git_prompt_showstashstate "true"

function fish_prompt
    set -l exit_code $status
    if echo "$name" | grep -q "^lorri"
        # lorri
        set_color cyan
        echo -n "lorri "
        set_color normal
    else
        # Nix shell
        if test -n "$IN_NIX_SHELL"
            set_color cyan
            echo -n "nix-shell "
            set_color normal
        end
        # direnv
        if test -n "$DIRENV_DIFF"
            set_color cyan
            echo -n "direnv "
            set_color normal
        end
    end
    # PWD
    set_color blue
    echo -n (prompt_pwd)" "
    set_color normal
    set -l git_branch ""
    # set -l hg_root_cmd (if command -v hg-root >/dev/null 2>&1; echo "hg-root"; else; echo "hg root"; end)
    if git rev-parse --git-dir >/dev/null 2>&1
        # Git
        set git_branch (git symbolic-ref --short HEAD 2>/dev/null; or git branch | head -n 1 | awk '{print $NF}' | tr -d ')')
        if test -n "$git_branch"
            set -l truncated (echo $git_branch | cut -c 1-35)
            set -l dirty (git status --porcelain)
            if test -z "$dirty"
                set_color green
            else
                if echo "$dirty" | rg -q "\?\?"
                    set_color red --bold
                else
                    set_color yellow
                end
            end
            if test "$git_branch" != "$truncated"
                echo -n "$truncated... "
            else
                echo -n "$git_branch "
            end
            set_color normal
        end
    # else if command -v hg >/dev/null 2>&1
    #     if eval "$hg_root_cmd" >/dev/null 2>&1
    #         # Mercurial
    #         set -l hg_info (hg prompt "{branch};{status};{status|unknown}")
    #         set -l hg_branch (echo $hg_info | cut -d ';' -f 1)
    #         set -l truncated (echo $hg_branch | cut -c 1-35)
    #         set -l hg_status (echo $hg_info | cut -d ';' -f 2)
    #         set -l hg_dirty (echo $hg_info | cut -d ';' -f 3)
    #         if test -z "$hg_status"
    #             set_color green
    #         else
    #             if test -n "$hg_dirty"
    #                 set_color red --bold
    #             else
    #                 set_color yellow
    #             end
    #         end
    #         if test "$hg_branch" != "$truncated"
    #             echo -n "$truncated... "
    #         else
    #             echo -n "$hg_branch "
    #         end
    #         set_color normal
    #     end
    end
    # Exit status
    if test $exit_code -ne 0
        set_color red
    end
    # Smart newline
    if test (math (tput cols) - \((echo \(prompt_pwd\)" $git_branch" | wc -c)\)) -lt 40
        echo
    end
    # Prompt character
    echo -n "λ "
    set_color normal
    # Ring bell
    # printf '\a'
end

# function fish_right_prompt
#   set_color black
#   date +%r
#   set_color normal
# end

# Colors
set fish_color_autosuggestion black
# set fish_color_cancel
set fish_color_command green
set fish_color_comment black
# set fish_color_cwd
# set fish_color_cwd_root
# set fish_color_end
set fish_color_error red
set fish_color_escape cyan
# set fish_color_history_current
# set fish_color_host
# set fish_color_match
set fish_color_normal normal
set fish_color_operator cyan
set fish_color_param normal
set fish_color_quote yellow
set fish_color_redirection cyan
# set fish_color_search_match
# set fish_color_selection
# set fish_color_user
set fish_color_valid_path --underline

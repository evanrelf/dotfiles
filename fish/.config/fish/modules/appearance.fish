# Prompt
function fish_greeting
    if test (uname) = "Linux" && _exists sway && test (tty) = "/dev/tty1"
        exec sway
    end
end
set -g fish_prompt_pwd_dir_length 1
# set __fish_git_prompt_showdirtystate "true"
# set __fish_git_prompt_showuntrackedfiles "true"
# set __fish_git_prompt_showstashstate "true"

# Add blank line between prompts, but not when the screen is cleared
# function newline_after --on-event fish_postexec
#     echo
# end

function fish_right_prompt
    # # nnn
    # if test -n "$NNNLVL"
    #     set_color black
    #     echo -n " "
    #     echo -n "nnn"
    #     if test "$NNNLVL" != "1"
    #         echo -n "$NNNLVL"
    #     end
    #     set_color normal
    # end
    # Nix
    if test -n "$IN_NIX_SHELL"
        # In Nix shell
        set_color cyan
        echo -n " nix-shell"
        set_color normal
    end
    set_color normal
end

function fish_prompt
    set -l exit_code $status
    # PWD
    set_color blue
    echo -n (prompt_pwd)" "
    set_color normal
    set -l git_branch ""
    # set -l hg_root_cmd (if _exists hg-root; echo "hg-root"; else; echo "hg root"; end)
    if _silently git rev-parse --git-dir
        # Git
        set git_branch (git symbolic-ref --short HEAD --quiet; or git branch | head -n 1 | awk '{print $NF}' | tr -d ')')
        if test -n "$git_branch"
            set -l truncated (echo $git_branch | cut -c 1-35)
            # Fast (ignores untracked files)
            # if git diff --quiet --ignore-submodules HEAD
            #     set_color green
            # else
            #     set_color yellow
            # end
            # Slow
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
    # else if _exists hg
    #     if _silently "$hg_root_cmd"
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
    # Split prompt onto two lines when it gets too long
    # if test (math (tput cols) - \((echo \(prompt_pwd\)" $git_branch" | wc -c)\)) -lt 40
    #     echo
    # end
    # echo
    # Prompt character
    echo -n "λ "
    set_color normal
    # Ring bell
    # printf '\a'
end

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

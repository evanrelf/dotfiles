if _exists fzf
    set --export FZF_LEGACY_KEYBINDINGS 0
    set --export FZF_DEFAULT_OPTS "--color=light --exact --height $FZF_TMUX_HEIGHT --reverse --ansi --no-multi"
    set --export FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
    set --export FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
    set --export FZF_CD_COMMAND "fd --type directory --follow --hidden --exclude '.git' --color=always"
    set --export FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND"
    set --export FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"

    function __fzf_find_rev -d "List Git revs"
        set --local commandline (__fzf_parse_commandline)
        set --local fzf_query $commandline[2]

        set --query FZF_FIND_REV_COMMAND || set --local FZF_FIND_REV_COMMAND "
        git log --graph --pretty=normal --first-parent --max-count=100 --color=always"

        begin
            eval "$FZF_FIND_REV_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_FIND_REV_OPTS --query \"$fzf_query\"" | while read -l s; set results $results $s; end
        end

        if test -z "$results"
            commandline -f repaint
            return
        else
            commandline --current-token ""
        end

        for result in $results
            commandline --insert --current-token -- (echo $result | cut -d ' ' -f 2)
            commandline --insert --current-token -- " "
        end
        commandline -f repaint
    end

    bind \cg __fzf_find_rev
end

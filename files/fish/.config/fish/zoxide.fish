if _exists zoxide
    set --export _ZO_DATA "$HOME/.cache/zoxide"
    if test -f "$HOME/.zo"
        rm "$HOME/.zo"
    end
    # Vendored from the official repo because I don't like the plugin is
    # configured by default
    function zoxide-add --on-event fish_prompt
        zoxide add
    end
    function z
        if test (count "$argv") -gt 0
            set --local _Z_RESULT (zoxide query "$argv")
            switch "$_Z_RESULT"
                case 'query: *'
                    cd (string sub -s 8 -- "$_Z_RESULT")
                    commandline -f repaint
                case '*'
                    echo -n "$_Z_RESULT"
            end
        end
    end
end

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
            if test -d "$_Z_RESULT"
              cd "$_Z_RESULT"
            else
              echo "$_Z_RESULT"
            end
        end
    end
end

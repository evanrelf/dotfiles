if _exists kak
    set --export EDITOR "kak"
else if _exists nvim
    set --export EDITOR "nvim"
else if _exists evil
    set --export EDITOR "evil"
else
    set --export EDITOR "vi"
end

# Kakoune daemon
# TODO: Fix issue where the daemon only has one working directory, so all
# clients use the same one. If you change the working directory of one client to
# be "correct", it messes up any other open clients.
function kakd
    if kak -c daemon -ui dummy -e quit >/dev/null 2>&1
        _error "Daemon already started"
        return 1
    else
        kak -d -s daemon &
        disown
    end
end
function kakc
    if not kak -c daemon $argv
        _log "Starting daemon..."
        kakd
        kak -c daemon $argv
    end
end

if _exists nvim
    function nvim
        if test -n "$TMUX"
            env TERM=screen-256color nvim $argv
        else
            command nvim $argv
        end
    end
end

# Magit as a standalone command, for use alongside other editors
if _exists evil
    function magit
        evil --eval "(magit-status)" $argv
    end
end

# Better support for Emacs' vterm package
function vterm_printf
    if test -n "$TMUX"
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

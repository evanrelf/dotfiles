alias reload "source $HOME/.config/fish/config.fish"

function abbr-erase
    set -g fish_user_abbreviations
    for a in (abbr --list)
        abbr --erase $a
    end
end

# ls
if _exists exa
    alias ls "exa --group-directories-first"
    alias ll "exa -l --group-directories-first"
    alias tree "exa --tree --group-directories-first -I '.git|dist-newstyle|.cache|.stack-work|elm-stuff|node_modules'"
else
    alias ls "ls -AFGh"
end

# Git
if _exists git; and status --is-interactive
    abbr --add g "git"
end
function r -d "Change directory to Git repository root"
    set -l root (git rev-parse --show-toplevel 2>/dev/null; or echo "")
    if test -n $root
        cd $root
    else
        return 1
    end
end

# Kakoune
if status --is-interactive
    abbr --add k "kak"
end
# function kakd-start
#     if command kak -l | grep "daemon" 2>&1 >/dev/null
#         _error "Daemon already running"
#     else
#         command kak -d -s daemon -ui dummy -E "daemon-init"
#     end
# end
# function kakd-stop
#     if command kak -l | grep "daemon" 2>&1 >/dev/null
#         command kak -c daemon -ui dummy -e "kill"
#     else
#         _error "Daemon not running"
#     end
# end
# function kakc
#     command kak -c daemon -e "client-init" $argv
#     if test $status -eq 255
#         _error "Daemon not running"
#     end
# end
complete --command kakc --wraps kak

# Emacs
if test -e "$HOME/.terminfo"
    alias emacs "env TERM=xterm-24bit emacs"
    alias emacsclient "env TERM=xterm-24bit emacsclient"
end

# nnn (from https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.fish)
if _exists nnn
    function nnn --description 'support nnn quit and change directory'
        # Block nesting of nnn in subshells
        if test -n "$NNNLVL"
            if test "$NNNLVL" -ge 1
                _log "nnn is already running"
                return
            end
        end

        # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
        # To cd on quit only on ^G, export NNN_TMPFILE after the call to nnn
        # NOTE: NNN_TMPFILE is fixed, should not be modified
        if test -n "$XDG_CONFIG_HOME"
            set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
        else
            set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
        end

        command nnn $argv

        if test -e $NNN_TMPFILE
            source $NNN_TMPFILE
            rm $NNN_TMPFILE
        end
    end
end

# Nix
if _exists nix
    function run
        if not test -f "shell.nix" && not test -f "default.nix"
            _error "Couldn't find 'shell.nix' or 'default.nix' in the current directory"
            return 1
        end
        _log "Entering Nix shell..."
        if test -z "$argv"
            nix-shell
        else
            nix-shell --command "$argv; return"
        end
    end
    function with
        if test -z "$argv"
            _error "with what?"
            return 1
        end
        _log "Entering Nix shell..."
        nix-shell --packages $argv
    end
    alias nix-stray-roots "nix-store --gc --print-roots | grep -vE '^(/nix/var/|\{censored)'"
end
if _exists cached-nix-shell
    alias "nix-shell" "cached-nix-shell"
end
if _exists lorri
    abbr --add losh "lorri shell"
end

# brain
if test \( -d "$HOME/brain" \) -a \( -f "$HOME/brain/new" \)
    alias new "$HOME/brain/new"
end

# Other
if _exists hub
    alias git "hub"
end
if status --is-interactive
    if _exists nvim
        abbr --add v "nvim"
    end
    if _exists nnn
        abbr --add n "nnn"
    end
    if _exists stack
        abbr --add sbf "stack build --fast"
        abbr --add cbf "cabal build -O0"
    end

    if _exists nix-env
        abbr --add pkgs "nix-env --install --file $HOME/.config/nix/packages.nix"
    end
end
function fn -d "Search for Haskell/PureScript function definition"
    set -l match (command rg --with-filename --ignore-case --line-number --multiline "^\s*(,|\{|let|where)?\s*\w*$argv[1]\w*\s+:: " -g "*.hs" -g "*.purs" | command fzf -1 -0 --height 50% --exact)
    set -l file (echo $match | command cut -d ':' -f 1)
    set -l line (echo $match | command cut -d ':' -f 2)
    if test -e $file
        eval $EDITOR $file +$line +"norm zz"
    else
        _error "No results found"
        return 1
    end
end
function rgl -d "Pipe ripgrep output to less"
    command rg --color always --heading --line-number --smart-case $argv | less --raw-control-chars
end
function treel -d "Pipe exa --tree output to less"
    command exa --color always --tree $argv | less --raw-control-chars
end

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
    alias ll "exa -l --group-directories-first --git"
    alias tree "exa --tree --group-directories-first -I '.git|.stack-work|elm-stuff'"
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
    abbr --add kc "kakc"
end
function kakd
    kak -c daemon -e 'kill'
    kak -d -s daemon 2>/dev/null
end
function kakc
    if test (count $argv) -eq 0
        kak -c daemon -e 'buffer *scratch*'
    else
        kak -c daemon $argv
    end
    if test $status -eq 255
        echo "Starting Kakoune daemon"
        kakd
        kakc $argv
    end
end
complete --command kakc --wraps kak

# Other
if _exists hub
    alias git "hub"
end
if status --is-interactive
    abbr --add n "nvim"
    if _exists stack
        abbr --add sbf "stack build --fast"
        abbr --add cbf "cabal build -O0"
    end
end
function fn -d "Search for Elm/Haskell function definition"
    set -l match (rg -Hin "^\s*(,|\{)?\s*\w*$argv[1]\w* ::? " -g "*.hs" -g "*.elm" | fzf -1 -0 --height 50% --exact)
    set -l file (echo $match | cut -d ':' -f 1)
    set -l line (echo $match | cut -d ':' -f 2)
    if test -e $file
        nvim $file +$line +"norm zz"
    else
        echo "No results found" >&2
        return 1
    end
end
function rgl -d "Pipe ripgrep output to less"
    rg --color always --heading --line-number --smart-case $argv | less --raw-control-chars
end
function treel -d "Pipe exa --tree output to less"
    exa --color always --tree $argv | less --raw-control-chars
end

alias reload "source $HOME/.config/fish/config.fish"

if _exists exa
    alias ls "exa --group-directories-first"
    alias ll "exa --long --group-directories-first"
    alias tree "exa --tree --group-directories-first --ignore-glob '.git|dist-newstyle|node_modules'"
else
    alias ls "command ls -AFGh"
end

if _exists kak
    abbr --add k "kak"
end

if _exists git
    abbr --add g "git"

    function root -d "Change directory to Git repository root"
        if set --local root (git rev-parse --show-toplevel 2>/dev/null)
            cd "$root"
        else
            _error "Not in Git repo"
            return 1
        end
    end
end

if _exists brew
    abbr --add cask "brew cask"
end

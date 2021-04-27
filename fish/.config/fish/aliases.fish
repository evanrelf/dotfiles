for a in (abbr --list)
    abbr --erase "$a"
end

alias reload "source $HOME/.config/fish/config.fish"

abbr --add mv "mv -i"
abbr --add cp "cp -i"

if _exists exa
    alias ls "exa --group-directories-first"
    alias ll "exa --long --group-directories-first"
    if not _exists tree
        alias tree "exa --tree --group-directories-first --ignore-glob '.git|dist-newstyle|node_modules'"
    end
else
    alias ls "command ls -AFGh"
end

if _exists kak
    abbr --add k "kak"
end

if _exists nvim
    abbr --add v "nvim"
end

if _exists evil
    abbr --add e "evil"
    abbr --add m "magit"
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

alias utcdate "date -u +'%Y-%m-%dT%H:%M:%S%Z'"

alias deathrow "sudo tree -x -C --prune / | grep -vE '(/etc/static|/persist|/nix/store)' | less"

function rg
    if isatty stdout
        command rg -p $argv | command less -RMFXK
    else
        command rg $argv
    end
end

alias less "less -RMK"

alias cargod "watchexec --exts rs --restart --clear -- cargo check --color=always '|&' less -RMK"

# Typos
abbr --add gs "git s"

for a in (abbr --list)
    abbr --erase "$a"
end

alias reload "source $HOME/.config/fish/config.fish"

abbr --add mv "mv -i"
abbr --add cp "cp -i"

if _exists fd && _exists as-tree
    function fdtree
        fd --hidden --exclude .git $argv | as-tree
    end
end

if _exists lsd
    alias ls "lsd --group-dirs=first --classify --icon=never"
    alias ll "ls -l"
else if _exists exa
    alias ls "exa --group-directories-first"
    alias ll "exa --long --group-directories-first"
    if not _exists tree
        alias tree "exa --tree --group-directories-first --ignore-glob '.git|dist-newstyle|node_modules'"
    end
else
    alias ls "command ls -AFGh"
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

if _exists tmux
    if test -n "$TMUX"
        alias tmux-copy "tmux load-buffer -"
        alias tmux-paste "tmux save-buffer -"
    end
end

alias utcdate "date -u +'%Y-%m-%dT%H:%M:%S%Z'"

if _exists nix
    abbr --add n "nix"
end

function rg
    if isatty stdout
        command rg -p $argv | command less -RMFXK
    else
        command rg $argv
    end
end

alias less "less -RMK"

function cargod
    if _exists cargo-clippy
        watchexec --exts rs --restart --clear -- cargo clippy --all-targets --color=always $argv '|&' less -~cRMK
    else
        watchexec --exts rs --restart --clear -- cargo check --all-targets --color=always $argv '|&' less -~cRMK
    end
end

# Typos
abbr --add gs "git s"

abbr --add grr "set -l commit (git k) && git rebase \$commit --exec 'env -i PATH=\$(dirname \$(which nix)) XDG_CACHE_HOME=.cache ./update-config --yes && git commit --all --fixup HEAD --allow-empty' && git rbauto \$commit"

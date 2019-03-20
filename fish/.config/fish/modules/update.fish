function update -d "Run all update commands"
    switch (uname)
        case Linux
            if _exists pacman
                # Arch Linux
                _log "Updating Arch Linux packages"
                sudo pacman -Syu
                sudo aura -Akua
            else if _exists nixos-version
                # NixOS
                _log "Updating NixOS packages"
                nix-channel --update
                sudo nix-channel --update
                nix-env --upgrade
                sudo nixos-rebuild switch --upgrade
            else if _exists apt
                # Ubuntu & Debian
                _log "Updating Ubuntu/Debian packages"
                sudo apt update
                sudo apt upgrade
            else if _exists dnf
                # Fedora & Red Hat
                _log "Updating Fedora/Red Hat packages"
                sudo dnf upgrade
            end
        case Darwin
            _log "Updating macOS software"
            softwareupdate -lia
            _exists mas
            and mas upgrade

            if _exists brew
                _log "Updating Homebrew packages"
                brew update
                brew upgrade
            end
        case '*'
            # Unknown OS
    end

    if _exists npm
        _log "Updating NPM packages"
        npm update -g
    end

    if _exists stack
        _log "Updating Haskell Stack packages"
        stack update
    end

    if _exists rustup
        _log "Updating Rust"
        rustup update
    end

    if _exists nvim -a -e $HOME/.local/share/nvim/site/autoload/plug.vim
        _log "Updating Neovim packages"
        nvim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
    end

    if _exists tldr
        _log "Updating tldr"
        tldr --update
    end

    if _exists jump
        _log "Updating jump directories"
        jump clean
    end

    if _exists autojump
        _log "Updating autojump directories"
        autojump --purge
    end

    _log "Updating Fish command completions"
    fish_update_completions
end

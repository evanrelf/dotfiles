function update -d "Run all update commands"
    switch (uname)
        case Linux
            # if _exists nixos-version
            #     _log "Updating NixOS channels"
            #     nix-channel --update
            #     sudo nix-channel --update
            #
            #     _log "Rebuilding NixOS system"
            #     sudo nixos-rebuild switch --upgrade
            # end
        case Darwin
            _log "Updating macOS software"
            softwareupdate -lia

            if _exists mas
                _log "Updating Mac App Store apps"
                mas upgrade
            end

            if _exists brew
                _log "Updating Homebrew formulae"
                brew update
                brew upgrade

                _log "Updating Homebrew casks"
                brew upgrade --cask
            end
    end

    # if _exists nix
    #     _log "Updating Nix channels"
    #     nix-channel --update
    #     sudo nix-channel --update
    #
    #     _log "Updating Nix packages"
    #     nix-env --upgrade
    #
    #     _log "Updating Nix search cache"
    #     nix search --update-cache | true
    # end

    if _exists npm
        _log "Updating NPM packages"
        # Fix error from missing lib directory
        test -d "$npm_config_prefix/lib" || mkdir "$npm_config_prefix/lib"
        npm update --global
    end

    if _exists stack
        _log "Updating Haskell Stack packages"
        stack update
    end

    if _exists cabal
        _log "Updating Cabal's Hackage package list"
        cabal update
    end

    if _exists rustup
        _log "Updating Rust"
        rustup update
    end

    # if _exists nvim -a -e $HOME/.local/share/nvim/site/autoload/plug.vim
    #     _log "Updating Neovim plugins"
    #     nvim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
    # end

    # if _exists doom
    #     doom upgrade --yes
    #     doom sync
    #     doom build
    # end

    if _exists tldr
        _log "Updating tldr"
        tldr --update
    end

    _log "Updating Fish command completions"
    fish_update_completions
end

function rc -d "Open the specified program's configuration file"
    if test (count $argv) -gt 0
        switch $argv[1]
            # Editors
            case vim vi
                eval $EDITOR "$HOME/.vimrc"
            case neovim nvim
                eval $EDITOR "$HOME/.config/nvim/init.vim"
            case kakoune kak
                cd "$HOME/.config/kak"
                ls -l
            case emacs
                eval $EDITOR "$HOME/.emacs"
            case spacemacs
                eval $EDITOR "$HOME/.spacemacs"

            # Shells
            case fish
                eval $EDITOR "$HOME/.config/fish/config.fish"
            case fisher fishfile
                eval $EDITOR "$HOME/.config/fish/fishfile"
            case zsh
                eval $EDITOR "$HOME/.zshrc"
            case bash
                eval $EDITOR "$HOME/.bashrc"

            # Window managers
            case xmonad
                eval $EDITOR "$HOME/.xmonad/xmonad.hs"
            case xmobar
                eval $EDITOR "$HOME/.config/xmobar/xmobarrc"
            case awesome
                eval $EDITOR "$HOME/.config/awesome/rc.lua"
            case polybar
                eval $EDITOR "$HOME/.config/polybar/config"
            case bspwm
                eval $EDITOR "$HOME/.config/bspwm/bspwmrc"
            case sxhkd
                eval $EDITOR "$HOME/.config/sxhkd/sxhkdrc"
            case compton
                eval $EDITOR "$HOME/.config/compton.conf"
            case hammerspoon
                eval $EDITOR "$HOME/.hammerspoon/init.lua"

            # Music
            case mpd
                eval $EDITOR "$HOME/.config/mpd/mpd.conf"
            case ncmpcpp
                eval $EDITOR "$HOME/.config/ncmpcpp/config"

            # Xorg
            case xresources
                eval $EDITOR "$HOME/.Xresources"
            case xprofile
                eval $EDITOR "$HOME/.xprofile"
            case xinit
                eval $EDITOR "$HOME/.xinitrc"

            # Other
            case tmux
                eval $EDITOR "$HOME/.tmux.conf"
            case git
                eval $EDITOR "$HOME/.config/git/config"
            case git-local
                eval $EDITOR "$HOME/.config/git/local"
            case alacritty
                eval $EDITOR "$HOME/.config/alacritty/alacritty.yml"
            case kitty
                eval $EDITOR "$HOME/.config/kitty/kitty.conf"
            case nixos
                eval $EDITOR "$HOME/dotfiles/nixos/configuration.nix"
                if _exists nixos-version
                    bash "$HOME/dotfiles/nixos/install"
                end
            case ranger
                eval $EDITOR "$HOME/.config/ranger/rc.conf"
            case zathura
                eval $EDITOR "$HOME/.config/zathura/zathurarc"
            case redshift
                eval $EDITOR "$HOME/.config/redshift/redshift.conf"

            case "*"
                _error "No config defined for '$argv[1]'"
        end
    else
        _error "No config specified"
    end
end
complete --command rc --require-parameter --no-files --arguments "mpd ncmpcpp chunkwm skhd vim neovim kakoune emacs compton spacemacs doom vscode fish fisher zsh bash bspwm sxhkd xmonad xmobar xresources xinit xprofile tmux git git-local hammerspoon alacritty kitty nixos redshift polybar"

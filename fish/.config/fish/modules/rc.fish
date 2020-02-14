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
                eval $EDITOR "$HOME/.emacs.d/init.el"
            case spacemacs
                eval $EDITOR "$HOME/.spacemacs"
            case doom
                cd "$HOME/.doom.d/"
                ls -l

            # Shells
            case fish
                cd "$HOME/.config/fish/modules/"
                ls -l
            case fisher fishfile
                eval $EDITOR "$HOME/.config/fish/fishfile"
            case zsh
                eval $EDITOR "$HOME/.zshrc"
            case bash
                eval $EDITOR "$HOME/.bashrc"
            case bash-profile
                eval $EDITOR "$HOME/.bash_profile"

            # Window managers
            case xmonad
                eval $EDITOR "$HOME/.xmonad/xmonad.hs"
            case xmobar
                eval $EDITOR "$HOME/.config/xmobar/xmobarrc"
            case taffybar
                eval $EDITOR "$HOME/.config/taffybar/taffybar.hs"
            case awesome
                eval $EDITOR "$HOME/.config/awesome/rc.lua"
            case polybar
                eval $EDITOR "$HOME/.config/polybar/config"
            case bspwm
                eval $EDITOR "$HOME/.config/bspwm/bspwmrc"
            case sxhkd
                eval $EDITOR "$HOME/.config/sxhkd/sxhkdrc"
            case sway
                eval $EDITOR "$HOME/.config/sway/config"
            case swaylock
                eval $EDITOR "$HOME/.config/swaylock/config"
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
            case mercurial hg
                eval $EDITOR "$HOME/.hgrc"
            case alacritty
                eval $EDITOR "$HOME/.config/alacritty/alacritty.yml"
            case kitty
                eval $EDITOR "$HOME/.config/kitty/kitty.conf"
            case nixos
                if test -e "$HOME/dotfiles/nixos/configuration.nix"
                  eval $EDITOR "$HOME/dotfiles/nixos/configuration.nix"
                  and if _exists nixos-version
                      bash "$HOME/dotfiles/nixos/install"
                  end
                else
                  eval $EDITOR "/etc/nixos/configuration.nix"
                end
            case home-manager home
                eval $EDITOR "$HOME/.config/nixpkgs/home.nix"
            case ranger
                eval $EDITOR "$HOME/.config/ranger/rc.conf"
            case zathura
                eval $EDITOR "$HOME/.config/zathura/zathurarc"
            case redshift
                eval $EDITOR "$HOME/.config/redshift/redshift.conf"
            case ghci
                eval $EDITOR "$HOME/.ghci"
            case broot
                if test (uname) = "Darwin"
                    eval $EDITOR "$HOME/Library/Preferences/org.dystroy.broot/conf.toml"
                else
                    _error "TODO: I don't know where the broot config lives on Linux..."
                end

            case "*"
                _error "No config defined for '$argv[1]'"
                return 1
        end
    else
        fd --type f --hidden --exclude ".git" . "$HOME/dotfiles/" | fzf --exact | xargs -o $EDITOR
    end
end
complete --command rc --require-parameter --no-files --arguments "vim vi neovim nvim kakoune kak emacs spacemacs doom fish fisher fishfile zsh bash bash-profile xmonad xmobar taffybar awesome polybar bspwm sxhkd sway swaylock compton hammerspoon mpd ncmpcpp xresources xprofile xinit tmux git git-local mercurial hg alacritty kitty nixos ranger zathura redshift ghci"

function rc -d "Open the specified program's configuration file"
    if test (count $argv) -gt 0
        switch $argv[1]
            # Text editors
            case vim vi
                eval "$EDITOR $HOME/.vimrc"
            case neovim nvim
                eval "$EDITOR $HOME/.config/nvim/init.vim"
            case kakoune kak
                cd "$HOME/.config/kak/"
                ls -l
            case emacs evil
                eval "$EDITOR $HOME/.config/emacs/init.el"
            case spacemacs
                eval "$EDITOR $HOME/.spacemacs"
            case doom
                cd "$HOME/.config/doom/"
                ls -l

                # Shells
            case fish
                cd "$HOME/.config/fish/"
                ls -l
            case fisher fishfile
                eval "$EDITOR $HOME/.config/fish/fishfile"
            case zshrc zsh
                eval "$EDITOR $HOME/.zshrc"
            case bashrc bash
                eval "$EDITOR $HOME/.bashrc"
            case bash-profile
                eval "$EDITOR $HOME/.bash_profile"

                # Version control
            case git
                eval "$EDITOR $HOME/.config/git/config"
            case git-local
                eval "$EDITOR $HOME/.config/git/local"
            case mercurial hg
                eval "$EDITOR $HOME/.hgrc"

                # Terminal emulators
            case alacritty
                eval "$EDITOR $HOME/.config/alacritty.yml"
            case kitty
                eval "$EDITOR $HOME/.config/kitty/kitty.conf"
            case kitty-local
                eval "$EDITOR $HOME/.config/kitty/local.conf"

                # Nix
            case nixos
                if test -e "$HOME/dotfiles/nixos/configuration.nix"
                    eval "$EDITOR $HOME/dotfiles/nixos/configuration.nix"
                    and if _exists nixos-version
                        bash "$HOME/dotfiles/nixos/install.sh"
                    end
                else
                    eval "$EDITOR /etc/nixos/configuration.nix"
                end
            case nix
                cd "$HOME/.config/nix/"
                ls -l
            case env
                eval "$EDITOR $HOME/.config/nix/env.nix"
            case home-manager home
                eval "$EDITOR $HOME/.config/nixpkgs/home.nix"
            case nix-darwin darwin
                eval "$EDITOR $HOME/.nixpkgs/darwin-configuration.nix"

                # Window managers and related tools
            case xmonad
                eval "$EDITOR $HOME/.xmonad/xmonad.hs"
                # eval "$EDITOR $HOME/.config/xmonad/xmonad.hs"
            case xmobar
                eval "$EDITOR $HOME/.config/xmobar/xmobarrc"
            case taffybar
                eval "$EDITOR $HOME/.config/taffybar/taffybar.hs"
            case awesome
                eval "$EDITOR $HOME/.config/awesome/rc.lua"
            case polybar
                eval "$EDITOR $HOME/.config/polybar/config"
            case bspwm
                eval "$EDITOR $HOME/.config/bspwm/bspwmrc"
            case sxhkd
                eval "$EDITOR $HOME/.config/sxhkd/sxhkdrc"
            case sway
                eval "$EDITOR $HOME/.config/sway/config"
            case swaylock
                eval "$EDITOR $HOME/.config/swaylock/config"
            case compton
                eval "$EDITOR $HOME/.config/compton.conf"
            case hammerspoon
                eval "$EDITOR $HOME/.config/hammerspoon/init.lua"
            case redshift
                eval "$EDITOR $HOME/.config/redshift/redshift.conf"
            case tmux
                eval "$EDITOR $HOME/.config/tmux/tmux.conf"

                # Xorg
            case xresources
                eval "$EDITOR $HOME/.Xresources"
            case xprofile
                eval "$EDITOR $HOME/.xprofile"
            case xinit
                eval "$EDITOR $HOME/.xinitrc"

                # Music players
            case mpd
                eval "$EDITOR $HOME/.config/mpd/mpd.conf"
            case ncmpcpp
                eval "$EDITOR $HOME/.config/ncmpcpp/config"

                # File browsers
            case ranger
                eval "$EDITOR $HOME/.config/ranger/rc.conf"
            case broot
                if test (uname) = "Darwin"
                    eval "$EDITOR $HOME/Library/Preferences/org.dystroy.broot/conf.toml"
                else
                    _error "TODO: I don't know where the broot config lives on Linux..."
                end

                # Other
            case ghci
                eval "$EDITOR $HOME/.ghci"
            case zathura
                eval "$EDITOR $HOME/.config/zathura/zathurarc"

                # Unknown
            case "*"
                _error "No config defined for '$argv[1]'"
                return 1
        end
    else
        fd --type file --hidden --exclude ".git" . "$HOME/dotfiles/" | fzf --exact | xargs -o "$EDITOR"
    end
end
complete --command rc --require-parameter --no-files --arguments "vim vi neovim nvim kakoune kak emacs spacemacs doom fish fisher fishfile zshrc zsh bashrc bash bash-profile git git-local mercurial hg alacritty kitty kitty-local nixos env home-manager home nix-darwin darwin xmonad xmobar taffybar awesome polybar bspwm sxhkd sway swaylock compton hammerspoon redshift tmux xresources xprofile xinit mpd ncmpcpp ranger broot ghci zathura"

function rc_generate_completions
    cat "$HOME/.config/fish/rc.fish" \
        | grep '^\s*case' \
        | sed 's/[[:space:]]*case //' \
        | grep -v '*' \
        | tr '\n' ' ' \
        | sed 's/ $//'
end

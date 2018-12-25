# vim: foldmethod=marker foldenable

# PLUGINS {{{1
# Auto-install fisher and plugins
if not functions -q fisher
    echo "Installing fisher..."
    curl https://git.io/fisher --create-dirs -sLo $HOME/.config/fish/functions/fisher.fish
    source $HOME/.config/fish/functions/fisher.fish
    fisher
end

set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git'"
set -U FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
set -U FZF_CD_COMMAND "fd --type directory --follow"
set -U FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
set -U FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"
set -U FZF_PREVIEW_FILE_COMMAND "bat --plain --color always --line-range :\$LINES"


# VARIABLES {{{1
set -x EDITOR "nvim"
set -x MANPAGER "nvim -c 'set ft=man' -"
set -x npm_config_prefix "$HOME/.node_modules"
set -x NNN_USE_EDITOR 1

if test (uname) = "Linux"
    set -x BROWSER "chromium"
end

set paths "$HOME/.local/bin" $paths
set paths "$HOME/.cargo/bin" $paths
set paths "$HOME/.node_modules/bin" $paths
if test (uname) = "Darwin"
    set paths "/usr/local/Cellar/node/11.0.0/bin" $paths
    set paths "/usr/local/sbin" $paths
    set paths "/usr/local/opt/python/libexec/bin" $paths
else if test (uname) = "Linux"
    set paths "$HOME/.gem/ruby/2.5.0/bin" $paths
end

for i in $paths
    if test -d $i
        set -x PATH "$i" $PATH
    end
end


# COMMANDS {{{1
alias reload "source $HOME/.config/fish/config.fish"

alias ed "pkill Emacs; pkill Emacs; emacs --daemon=term; emacs --daemon=gui"
alias et "emacsclient -s term -t"
alias eg "emacsclient -s gui -c -n"

function kd
    command kak -c daemon -e 'kill'
    command kak -d -s daemon 2>/dev/null
end

function kc
    if test (count $argv) -eq 0
        command kak -c daemon -e 'buffer *scratch*'
    else
        command kak -c daemon $argv
    end
end
complete -c kc -w kak

alias g "git"
alias scrot "command scrot --silent"
alias lock "systemctl suspend; and physlock -d"

if test (command -s exa)
    alias ls "exa --group-directories-first"
    alias ll "exa -l --group-directories-first"
    alias tree "exa --tree --group-directories-first -I '.git|.stack-work|elm-stuff'"
else
    alias ls "ls -AFGh"
end

if test (uname) = "Darwin"
    alias cask "brew cask"

    if test (command -s gittower)
        alias tower "gittower ."
    end

    if test -e /Applications/Marked\ 2.app
        alias marked "open -a Marked\ 2.app"
    end
end

if test (command -s pnpm)
    alias npm "pnpm"
end

if status --is-interactive
    set -g fish_user_abbreviations
    if test (command -s stack)
        abbr --add ghc "stack ghc"
        abbr --add ghci "stack ghci"
        abbr --add runghc "stack runghc"
    end
end

alias qa "~/Code/scripts/qa/qa"
alias vpn "~/Code/scripts/vpn/vpn"
alias gauntlet "~/Code/scripts/gauntlet/gauntlet"
alias sql "psql -d vetpro -p 5432 -h localhost -U postgres"

# update - Run all update commands {{{2
function update -d "Run all update commands"
    switch (uname)
        case Linux
            if test (command -s pacman)
                # Arch Linux
                set_color yellow
                echo "== Updating Arch Linux packages"
                set_color normal
                sudo pacman -Syu
                sudo aura -Akua
            else if test (command -s nix-env)
                # NixOS
                set_color yellow
                echo "== Updating NixOS derivations"
                set_color normal
                nix-channel --update
                nix-env --upgrade
                sudo nixos-rebuild switch --upgrade
            else if test (command -s apt)
                # Ubuntu & Debian
                set_color yellow
                echo "== Updating Ubuntu/Debian packages"
                set_color normal
                sudo apt update
                sudo apt upgrade
            else if test (command -s dnf)
                # Fedora & Red Hat
                set_color yellow
                echo "== Updating Fedora/Red Hat packages"
                set_color normal
                sudo dnf upgrade
            end
        case Darwin
            set_color yellow
            echo "== Updating macOS software"
            set_color normal
            softwareupdate -lia
            test (command -s mas)
            and mas upgrade

            if test (command -s brew)
                set_color yellow
                echo "== Updating Homebrew packages"
                set_color normal
                brew update
                brew upgrade
            end
        case '*'
            # nothing
    end

    if test (command -s npm)
        set_color yellow
        echo "== Updating NPM packages"
        set_color normal
        npm update -g
    end

    if test (command -s stack)
        set_color yellow
        echo "== Updating Haskell Stack packages"
        set_color normal
        stack update
    end

    if test (command -s rustup)
        set_color yellow
        echo "== Updating Rust"
        set_color normal
        rustup update
    end

    if test (command -s nvim) -a -e $HOME/.config/nvim/autoload/plug.vim
        set_color yellow
        echo "== Updating Neovim packages"
        set_color normal
        nvim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
    else if test (command -s vim) -a -e $HOME/.vim/autoload/plug.vim
        set_color yellow
        echo "== Updating Vim packages"
        set_color normal
        vim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
    end

    if test (command -s tldr)
        set_color yellow
        echo "== Updating tldr"
        set_color normal
        tldr --update
    end

    set_color yellow
    echo "== Updating Fish command completions"
    set_color normal
    fish_update_completions
end
# }}}2
# iso2img - Convert an ISO to an IMG {{{2
function iso2img -d "Convert an ISO to an IMG"
    if test (count $argv) -gt 0
        for iso in $argv
            hdiutil convert -format UDRW -o "$iso.img" "$iso"
            and mv "$iso.img.dmg" "$iso.img"
            and mv "$iso.img" (echo "$iso.img" | sed "s/\.iso//")
        end
    else
        echo "No ISO files specified" >&2
        return 1
    end
end
complete --command iso2img --require-parameter
# }}}2
# keeb - Safely flash QMK firmware to TADA68 keyboard {{{2
function keeb -d "Safely flash QMK firmware to TADA68 keyboard"
    set -l qmk_dir "$HOME/Projects/qmk_firmware"
    set -l mount_dir "/Volumes/TADA68  "

    function error
        set_color red
        echo "ERROR: $argv" >&2
        set_color normal
    end

    if test ! -d "$mount_dir"
        error "Volume not mounted at '$mount_dir'"
        return 1
    end

    if test ! -d "$qmk_dir"
        error "QMK directory '$qmk_dir' does not exist"
        return 1
    else
        cd $qmk_dir
        or "Failed to change directory to '$qmk_dir'"
    end

    if git remote -v | not grep "qmk_firmware" >/dev/null
        error "Invalid QMK directory: '$qmk_dir'"
        return 1
    end

    if not command -s nix-shell >/dev/null
        error "Nix not installed"
        return 1
    else
        nix-shell --arg arm false --command "make tada68:evanrelf"
        if test $status -ne 0
            error "Failed to compile firmware"
            return 1
        end
    end

    for file in (command ls -A "$mount_dir")
        command rm -rf "$mount_dir/$file"
        if test $status -ne 0
            error "Failed to remove '$file' from volume"
            return 1
        end
    end

    if test ! -e "tada68_evanrelf.bin"
        error "Firmware file 'tada68_evanrelf.bin' does not exist"
        return 1
    else
        cp "tada68_evanrelf.bin" "$mount_dir/FLASH.BIN"
        if test $status -ne 0
            error "Failed to copy firmware to volume"
            return 1
        end
    end

    set -l files (command ls -A "$mount_dir")
    if test $files = "FLASH.BIN"
        echo
        set_color green
        echo "=== SUCCESS ==="
        set_color yellow
        echo "DO NOT EJECT DRIVE ON COMPUTER"
        echo "PRESS ESCAPE KEY ON KEYBOARD TO FINISH"
    end

    cd -

    set_color normal
end
# }}}2
# pman - Open man page as PDF in Preview {{{2
function pman -d "Open man page as PDF in Preview" -w man
    man -t $argv[1] | open -f -a Preview
end
# }}}2
# r - cd to project root {{{2
function r -d "cd to project root"
    set -l root (git rev-parse --show-toplevel 2>/dev/null; or echo "")
    test -n $root
    and cd $root
    or return 1
end
# }}}2
# rc - Open the specified program's configuration file {{{2
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
            case spacemacs emacs-spacemacs
                eval $EDITOR "$HOME/.spacemacs"
            case vscode
                eval $EDITOR "$HOME/Library/Application\ Support/Code/User/settings.json"
            case yi
                cd "$HOME/dotfiles/yi"

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
                eval $EDITOR "$HOME/.xmobarrc"
            case polybar
                eval $EDITOR "$HOME/.config/polybar/config"
            case bspwm
                eval $EDITOR "$HOME/.config/bspwm/bspwmrc"
            case sxhkd
                eval $EDITOR "$HOME/.config/sxhkd/sxhkdrc"
            case compton
                eval $EDITOR "$HOME/.config/compton.conf"

                # Xorg
            case xresources
                eval $EDITOR "$HOME/.Xresources"
            case xinit
                eval $EDITOR "$HOME/.xinitrc"

                # Other
            case tmux
                eval $EDITOR "$HOME/.tmux.conf"
            case git
                eval $EDITOR "$HOME/.gitconfig"
            case git-local
                eval $EDITOR "$HOME/.gitconfig.local"
            case hammerspoon
                eval $EDITOR "$HOME/.hammerspoon/init.lua"
            case alacritty
                eval $EDITOR "$HOME/.config/alacritty/alacritty.yml"
            case kitty
                eval $EDITOR "$HOME/.config/kitty/kitty.conf"
            case nixos
                sudoedit "/etc/nixos/configuration.nix"
            case ranger
                eval $EDITOR "$HOME/.config/ranger/rc.conf"
            case zathura
                eval $EDITOR "$HOME/.config/zathura/zathurarc"
            case redshift
                eval $EDITOR "$HOME/.config/redshift/redshift.conf"

            case "*"
                set_color red
                echo "No config defined for '$argv[1]'" >&2
                set_color normal
                return 1
        end
    else
        set_color red
        echo "No config specified" >&2
        set_color normal
        return 1
    end
end
complete --command rc --require-parameter --no-files --arguments "vim neovim kakoune emacs compton spacemacs doom vscode fish fisher zsh bash bspwm sxhkd xmonad xmobar xresources xinit tmux git git-local hammerspoon alacritty kitty nixos redshift polybar"
# }}}2
# refresh - Restart system applications {{{2
function refresh -d "Restart system applications"
    # defaults write com.apple.dock ResetLaunchPad -bool true 2>/dev/null
    # or echo "Failed to kill SystemUIServer" >&2
    killall SystemUIServer 2>/dev/null
    or echo "Failed to kill SystemUIServer" >&2
    killall Dock 2>/dev/null
    or echo "Failed to kill Dock" >&2
    killall Finder 2>/dev/null
    or echo "Failed to kill Finder" >&2
    killall ControlStrip 2>/dev/null
    or echo "Failed to kill ControlStrip" >&2
end
# }}}2
# rmds - Remove .DS_Store files recursively from current directory {{{2
function rmds -d "Remove .DS_Store files recursively from current directory"
    if test (command -s fd)
        echo "Searching..."
        set -l files (fd --hidden --no-ignore --case-sensitive --absolute-path --exclude '.Trash' .DS_Store)
        if test (count $files) -gt 0
            for i in $files
                rm "$i"
                and echo "Removed $i"
                or echo "* Failed to remove $i" >&2
            end
        else
            echo "No .DS_Store files found" >&2
            return 1
        end
    else
        echo "fd not installed" >&2
        return 1
    end
end
# }}}2
# V - Fuzzy file open in editor {{{2
function V -d "Fuzzy file open in editor"
    # Arguments to the function are passed to fzf as exact phrases
    set -l queries
    # for arg in $argv
    #     set queries $queries "'$arg"
    # end
    set -l fd_cmd "fd --type file --follow --hidden --exclude '.git'"
    set -l fzf_cmd "fzf -1 -0 --height=30% --exact -q \"$queries \""
    set -l file (eval "$fd_cmd | $fzf_cmd")
    if test $status -eq 130
        return 0
    end
    test -n "$file"
    and echo "Opening $file"
    and eval $EDITOR $file
    or return 1
end
# }}}2
# v - Fuzzy project file open in editor {{{2
function v -d "Fuzzy project file open in editor"
    set -l before (pwd)
    r
    set -l after (pwd)
    V $argv
    if test $before != $after
        cd -
    end
end
# }}}2
# C - Fuzzy directory changer {{{2
function C -d "Fuzzy directory changer"
    # Arguments to the function are passed to fzf as exact phrases
    set -l queries
    # for arg in $argv
    #     set queries $queries "'$arg"
    # end
    set -l fd_cmd "fd --type directory --follow --hidden --exclude '.git'"
    set -l fzf_cmd "fzf -1 -0 --height=30% --exact -q \"$queries \""
    set -l dir (eval "$fd_cmd | $fzf_cmd")
    if test $status -eq 130
        return 0
    end
    test -n "$dir"
    and cd $dir
    or return 1
end
# }}}2
# c - Fuzzy project directory changer {{{2
function c -d "Fuzzy project directory changer"
    r
    C $argv
end
# }}}2
# wig - WireGuard {{{2
function wig -d "WireGuard"
    set -l interface "mullvad-us2"

    switch $argv[1]
        case u up start
            sudo wg-quick up $interface
        case d down stop
            sudo wg-quick down $interface
        case s status
            wg show >&2 2>/dev/null
            and echo "Down"
            or echo "Up"
        case "*"
            echo "Invalid command '$argv[1]'" >&2
            return 1
    end
end
# }}}2


# PROMPT {{{1
set -g fish_greeting ""
set -g fish_prompt_pwd_dir_length 0
set __fish_git_prompt_showdirtystate "true"
set __fish_git_prompt_showuntrackedfiles "true"
set __fish_git_prompt_showstashstate "true"

function fish_prompt
    set -l exit_code $status
    # PWD
    set_color blue
    echo -n (prompt_pwd)" "
    set_color normal
    # virtualenv
    if set -q VIRTUAL_ENV
        set_color cyan
        echo -n -s "(" (basename "$VIRTUAL_ENV") ") "
        set_color normal
    end
    # Git
    set -l git_dir (git rev-parse --git-dir 2> /dev/null)
    if test -n "$git_dir"
        set -l branch (git symbolic-ref --short HEAD 2>/dev/null)
        if test -n "$branch"
            set -l truncated (echo $branch | cut -c 1-25)
            set -l dirty (git status --porcelain)
            if test -z "$dirty"
                set_color green
            else
                if echo "$dirty" | rg -q "\?\?"
                    set_color red --bold
                else
                    set_color yellow
                end
            end
            if test "$branch" != "$truncated"
                echo -n "$truncated... "
            else
                echo -n "$branch "
            end
            set_color normal
        end
    end
    # Exit status
    if test $exit_code -ne 0
        set_color red
    end
    # Prompt character
    echo -n "λ "
    set_color normal
end


# COLORS {{{1
set fish_color_command green
set fish_color_param normal
set fish_color_quote cyan
set fish_color_error red
set fish_color_valid_path --underline
set fish_color_comment black --bold
set fish_color_autosuggestion black --bold


# EXTRAS {{{1
# Nix {{{2
if test -e $HOME/.nix-profile/etc/profile.d/nix.sh
    if type -q bass
        bass source $HOME/.nix-profile/etc/profile.d/nix.sh
    else
        echo "Nix isn't working because you don't have bass"
    end
end
# }}}2
# virtualfish {{{2
# if test (command -s python3)
#   eval (python3 -m virtualfish)
# end

# xinit {{{2
if test (tty) = "/dev/tty1"
    if test (command -s startx)
        startx
    end
end

# }}}2


# }}}1

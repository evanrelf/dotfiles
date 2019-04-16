# IMPORTS {{{1

source $HOME/.config/fish/prelude.fish
source $HOME/.config/fish/modules/emacs.fish
source $HOME/.config/fish/modules/git.fish
source $HOME/.config/fish/modules/iso2img.fish
source $HOME/.config/fish/modules/kakoune.fish
source $HOME/.config/fish/modules/keeb.fish
if test (uname) = "Darwin"
    source $HOME/.config/fish/modules/macos.fish
else if uname -a | grep -q NixOS
    source $HOME/.config/fish/modules/nixos.fish
else if uname -a | grep -q ARCH
    source $HOME/.config/fish/modules/arch.fish
end
source $HOME/.config/fish/modules/panosoft.fish
source $HOME/.config/fish/modules/rc.fish
source $HOME/.config/fish/modules/rmds.fish
source $HOME/.config/fish/modules/search.fish
source $HOME/.config/fish/modules/update.fish
source $HOME/.config/fish/modules/wireguard.fish


# PLUGINS {{{1
# Auto-install fisher and plugins
if not functions -q fisher
    echo "Installing fisher..."
    curl https://git.io/fisher --create-dirs -sLo $HOME/.config/fish/functions/fisher.fish
    source $HOME/.config/fish/functions/fisher.fish
    fisher
end

set -x FZF_LEGACY_KEYBINDINGS 0
set -x FZF_DEFAULT_OPTS "--exact --height $FZF_TMUX_HEIGHT"
set -x FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git'"
set -x FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
set -x FZF_CD_COMMAND "fd --type directory --follow"
set -x FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
set -x FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"


# VARIABLES {{{1
set -x EDITOR "nvim"
set -x MANPAGER "nvim -c 'set ft=man' -"
set -x npm_config_prefix "$HOME/.node_modules"
set -x NNN_USE_EDITOR 1

if test (uname) = "Linux"
    set -x BROWSER "chromium"
end

set paths "$HOME/.local/bin" $paths
set paths "$HOME/.config/git/scripts" $paths
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

if _exists exa
    alias ls "exa --group-directories-first"
    alias ll "exa -l --group-directories-first --git"
    alias tree "exa --tree --group-directories-first -I '.git|.stack-work|elm-stuff'"
else
    alias ls "ls -AFGh"
end

if _exists hub
    alias git "hub"
end

if status --is-interactive
    # set -g fish_user_abbreviations
    abbr --add n "nvim"
    if _exists stack
        abbr --add ghc "stack ghc"
        abbr --add ghci "stack ghci"
        abbr --add runghc "stack runghc"
        abbr --add ghcid "ghcid --command 'stack ghci'"
    end
end


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
    # Git
    set -l branch ""
    set -l git_dir (git rev-parse --git-dir 2> /dev/null)
    if test -n "$git_dir"
        set branch (git symbolic-ref --short HEAD 2>/dev/null; or git branch | head -n 1 | awk '{print $NF}' | tr -d ')')
        if test -n "$branch"
            set -l truncated (echo $branch | cut -c 1-35)
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
    # Smart newline
    if test (math (tput cols) - \((echo \(prompt_pwd\)" $branch" | wc -c)\)) -lt 40
        echo
    end
    # Prompt character
    echo -n "λ "
    set_color normal
    # Ring bell
    # printf '\a'
end

# function fish_right_prompt
#   set_color black
#   date +%r
#   set_color normal
# end


# COLORS {{{1
set fish_color_autosuggestion black
# set fish_color_cancel
set fish_color_command green
set fish_color_comment black
# set fish_color_cwd
# set fish_color_cwd_root
# set fish_color_end
set fish_color_error red
# set fish_color_escape
# set fish_color_history_current
# set fish_color_host
# set fish_color_match
# set fish_color_normal
# set fish_color_operator
set fish_color_param normal
set fish_color_quote yellow --bold
# set fish_color_redirection
# set fish_color_search_match
# set fish_color_selection
# set fish_color_user
set fish_color_valid_path --underline


# EXTRAS {{{1
# Nix {{{2
if test -e $HOME/.nix-profile/etc/profile.d/nix.sh
    if type -q bass
        bass source $HOME/.nix-profile/etc/profile.d/nix.sh
    else
        _error "Nix isn't working because you don't have bass"
    end
end

# No display manager {{{2
if test (tty) = "/dev/tty1"; and _exists startx
    # if _exists sway
    #     sway
    # else
        startx
    # end
    exit 0
end

# }}}2


# vim: foldmethod=marker foldenable

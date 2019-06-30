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

set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_DEFAULT_OPTS "--exact --height $FZF_TMUX_HEIGHT --reverse --ansi"
set -U FZF_DEFAULT_COMMAND "fd --type file --follow --hidden --exclude '.git' --color=always"
set -U FZF_FIND_FILE_COMMAND "$FZF_DEFAULT_COMMAND"
set -U FZF_CD_COMMAND "fd --type directory --follow"
set -U FZF_CD_WITH_HIDDEN_COMMAND "$FZF_CD_COMMAND --hidden --exclude '.git'"
set -U FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"


# VARIABLES {{{1
set -x EDITOR "nvim"
# set -x EDITOR "emacsclient -s term -t"
# set -x EDITOR "kak"
set -x MANPAGER "nvim -c 'set ft=man' -"
set -x npm_config_prefix "$HOME/.node_modules"
set -x NNN_USE_EDITOR 1
set -x PSVM_HOME "$HOME/.config/psvm"

if test (uname) = "Linux"
    set -x BROWSER "chromium"
end

# set paths "$HOME/.cabal/bin" $paths
# set paths "$HOME/.ghcup" $paths
set paths "$HOME/.local/bin" $paths
set paths "$HOME/.config/git/scripts" $paths
set paths "$HOME/.emacs.d/bin" $paths
set paths "$HOME/.cargo/bin" $paths
set paths "$HOME/.node_modules/bin" $paths
set paths "$PSVM_HOME/current/bin" $paths
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


# KEYMAPPINGS {{{1

function hybrid_bindings
    # fish_default_key_bindings -M default
    fish_default_key_bindings -M insert
    # fish_default_key_bindings -M visual
    fish_vi_key_bindings --no-erase
end

set -U fish_cursor_default block
set -U fish_cursor_unknown block
set -U fish_cursor_insert line
set -U fish_cursor_visual block

# Default
fish_default_key_bindings
# Hybrid
# set -U fish_key_bindings hybrid_bindings
# Vim
# fish_vi_key_bindings --no-erase

# Uncomment this to use a line cursor shape with the default keybindings
function fish_default_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
    __fish_cursor_xterm line
end

function fish_default_cursor_handle_preexec --on-event fish_preexec
    __fish_cursor_xterm line
end

# Uncomment this to use a dynamic cursor shape with the hybrid or vim keybindings
# if test $TERM = "xterm-kitty"
#     function fish_vi_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
#         set -l varname fish_cursor_$fish_bind_mode
#         if not set -q $varname
#             set varname fish_cursor_unknown
#         end
#         __fish_cursor_xterm $$varname
#     end
#     function fish_vi_cursor_handle_preexec --on-event fish_preexec
#         set -l varname fish_cursor_default
#         if not set -q $varname
#             set varname fish_cursor_unknown
#         end
#         __fish_cursor_xterm $$varname
#     end
# else
#     fish_vi_cursor
# end


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
    # for a in (abbr --list)
    #     abbr --erase $a
    # end
    abbr --add n "nvim"
    if _exists stack
        # abbr --add ghc "stack ghc"
        # abbr --add runghc "stack runghc"
        abbr --add ghci "stack ghci"
        abbr --add sbf "stack build --fast"
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
set fish_color_autosuggestion brblack
# set fish_color_cancel
set fish_color_command green
set fish_color_comment black
# set fish_color_cwd
# set fish_color_cwd_root
# set fish_color_end
set fish_color_error red
set fish_color_escape cyan
# set fish_color_history_current
# set fish_color_host
# set fish_color_match
set fish_color_normal normal
set fish_color_operator cyan
set fish_color_param normal
set fish_color_quote yellow
set fish_color_redirection cyan
# set fish_color_search_match
# set fish_color_selection
# set fish_color_user
set fish_color_valid_path --underline


# EXTRAS {{{1

# Stack auto-completion {{{2
function _stack
    set -l cl (commandline --tokenize --current-process)
    # Hack around fish issue #3934
    set -l cn (commandline --tokenize --cut-at-cursor --current-process)
    set -l cn (count $cn)
    set -l tmpline --bash-completion-enriched --bash-completion-index $cn
    for arg in $cl
        set tmpline $tmpline --bash-completion-word $arg
    end
    for opt in (stack $tmpline)
        if test -d $opt
            echo -E "$opt/"
        else
            echo -E "$opt"
        end
    end
end

complete --no-files --command stack --arguments '(_stack)'

# ghcup {{{2
if test -e $HOME/.ghcup/env
    if type -q bass
        bass source $HOME/.ghcup/env
    else
        _error "ghcup isn't working because you don't have bass"
    end
end

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

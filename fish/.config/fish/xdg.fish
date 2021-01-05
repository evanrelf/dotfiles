set --export XDG_CONFIG_HOME "$HOME/.config"
set --export XDG_CACHE_HOME "$HOME/.cache"
set --export XDG_DATA_HOME "$HOME/.local/share"

# From https://wiki.archlinux.org/index.php/XDG_Base_Directory

# aws-cli
if _exists aws-cli
    set --export AWS_SHARED_CREDENTIALS_FILE "$XDG_CONFIG_HOME/aws/credentials"
    set --export AWS_CONFIG_FILE "$XDG_CONFIG_HOME/aws/config"
end

# bash
if _exists bash
    set --export HISTFILE "$XDG_DATA_HOME/bash/history"
    if test ! -d "$XDG_DATA_HOME/bash"
        mkdir -p "$XDG_DATA_HOME/bash"
    end
end

# cargo
if _exists cargo
    set --export CARGO_HOME "$XDG_DATA_HOME/cargo"
end

# gnupg
# if _exists gpg
#     set --export GNUPGHOME "$XDG_DATA_HOME/gnupg"
# end

# go
if _exists go
    set --export GOPATH "$XDG_DATA_HOME/go"
end

# gtk
set --export GTK_RC_FILES "$XDG_CONFIG_HOME/gtk-1.0/gtkrc"
set --export GTK2_RC_FILES "$XDG_CONFIG_HOME/gtk-2.0/gtkrc"

# kde
set --export KDEHOME "$XDG_CONFIG_HOME/kde"

# less
if _exists less
    set --export LESSHISTFILE "-"
end

# ncurses
set --export TERMINFO "$XDG_DATA_HOME/terminfo"
set --export TERMINFO_DIRS "$XDG_DATA_HOME/terminfo:/usr/share/terminfo"

# npm
if _exists npm
    set --export NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/npm/npmrc"
end

# stack
if _exists stack
    set --export STACK_ROOT "$XDG_DATA_HOME/stack"
end

# taskwarrior
if _exists task
    set --export TASKDATA "$XDG_DATA_HOME/task"
    set --export TASKRC "$XDG_CONFIG_HOME/task/taskrc"
    if test ! -d "$TASKDATA"
        mkdir -p "$TASKDATA"
    end
end

# xorg
if _exists startx
    set --export XAUTHORITY "$XDG_RUNTIME_DIR/Xauthority"
end

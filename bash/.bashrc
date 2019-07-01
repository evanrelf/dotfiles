#!/usr/bin/env bash

MANPATH="${MANPATH:""}"

set -euo pipefail
set +x

# IMPORTS {{{1
if [ -f "$HOME/local/share/bash/sensible.bash" ]; then
  # shellcheck source=/dev/null
  source "$HOME/local/share/bash/sensible.bash"
fi

if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
  # shellcheck source=/dev/null
  source "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi


# VARIABLES {{{1
export EDITOR="nvim"
export MANPAGER="nvim -c 'set ft=man' -"
export npm_config_prefix="$HOME/.node_modules"
export PSVM_HOME="$HOME/.config/psvm"

if [ "$(uname)" = "Linux" ]; then
  export BROWSER="chromium"
fi


# PATH {{{1
if [ "$(uname)" = "Darwin" ]; then
  export PATH="/usr/local/Cellar/node/11.0.0/bin:$PATH"
  export PATH="/usr/local/sbin:$PATH"
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
elif [ "$(uname)" = "Linux" ]; then
  export PATH="$HOME/.gem/ruby/2.5.0/bin:$PATH"
fi
export PATH="$HOME/.cargo/bin:$PATH"
# export PATH="$HOME/.cabal/bin:$PATH"
# export PATH="$HOME/.ghcup:$PATH"
# export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.node_modules/bin:$PATH"
export PATH="$PSVM_HOME/current/bin:$PATH"
export PATH="$HOME/.config/git/scripts:$PATH"
export PATH="$HOME/.local/bin/:$PATH"


# ALIASES {{{1
alias reload="source \$HOME/.bash_profile"

if command -v exa >/dev/null 2>&1; then
  alias ls="exa --group-directories-first"
  alias ll="exa -l --group-directories-first --git"
  alias tree="exa --tree --group-directories-first -I '.git|.stack-work|elm-stuff'"
else
  alias ls="ls -AFGh"
fi

if command -v hub >/dev/null 2>&1; then
  alias git="hub"
fi

alias n="nvim"
alias sbf="stack build --fast"


# PROMPT {{{1
PS1="\w λ "


# vim: foldmethod=marker foldenable

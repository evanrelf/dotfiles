# https://github.com/zsh-users/zsh-syntax-highlighting
# https://github.com/zsh-users/zsh-history-substring-search
# https://github.com/zsh-users/zsh-autosuggestions
# https://github.com/marlonrichert/zsh-autocomplete
# https://github.com/olets/zsh-abbr

export ZDOTDIR="$HOME/.config/zsh"
if [ ! -d "$XDG_DATA_HOME/zsh" ]; then
  mkdir -p "$XDG_DATA_HOME/zsh"
fi
export HISTFILE="$XDG_DATA_HOME/zsh/history"

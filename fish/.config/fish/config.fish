# vim foldmethod=marker foldenable

# VARIABLES {{{1
set -x EDITOR nvim
set -x GPG_TTY (tty)

set -l paths $HOME/.cargo/bin $HOME/.local/bin
for i in $paths
  if test -d $i
    set -x PATH $i $PATH
  end
end


# COMMANDS {{{1
# alias ls "ls -AFGh"
alias ls "exa -aF --group-directories-first"
alias ll "ls -l"
alias lsa "exa -a --group-directories-first"
alias rm "trash"
alias git "hub"
alias refresh "killall SystemUIServer; killall Dock; killall ControlStrip; pkill \"Touch Bar agent\""
alias reload "source $HOME/.config/fish/config.fish"
alias tower "gittower ."

# wm - Manage tiling window manager {{{2
function wm -d "Manage tiling window manager"
  if test (count $argv) -gt 0
    switch $argv[1]
    case start enable
      brew services start chunkwm
      brew services start skhd
    case stop disable
      brew services stop chunkwm
      brew services stop skhd
    case restart
      brew services restart chunkwm
      brew services restart skhd
    case "*"
      echo Not defined: $argv[1]
    end
  else
    echo No argument
  end
end
complete --command wm --require-parameter --no-files --arguments "start stop restart"

# iso2img - Convert an ISO to an IMG {{{2
function iso2img -d "Convert an ISO to an IMG"
  for iso in $argv
    hdiutil convert -format UDRW -o "$iso.img" "$iso"
    mv "$iso.img.dmg" "$iso.img"
    mv "$iso.img" (echo "$iso.img" | sed "s/\.iso//")
  end
end
complete --command iso2img --require-parameter

# rc - Open the specified program's configuration file {{{2
function rc -d "Open the specified program's configuration file"
  if test (count $argv) -gt 0
    switch $argv[1]
      # Editors
      case vim vi
        eval $EDITOR $HOME/.vimrc
      case neovim nvim
        eval $EDITOR $HOME/.config/nvim/init.vim
      case kakoune kak
        eval $EDITOR $HOME/.config/kak/kakrc

      # Terminals
      case alacritty
        eval $EDITOR $HOME/.config/alacritty/alacritty.yml
      case kitty
        eval $EDITOR $HOME/.kitty.conf

      # Window manager
      case chunkwm
        eval $EDITOR $HOME/.chunkwmrc
      case skhd
        eval $EDITOR $HOME/.skhdrc
      case hammerspoon
        eval $EDITOR $HOME/.hammerspoon/init.lua

      # Shells
      case fish
        eval $EDITOR $HOME/.config/fish/config.fish
      case zsh
        eval $EDITOR $HOME/.zshrc
      case bash
        eval $EDITOR $HOME/.bashrc

      # Other
      case git
        eval $EDITOR $HOME/.gitconfig
      case ranger
        eval $EDITOR $HOME/.config/ranger/rc.conf

      case "*"
        echo Not defined: $argv[1]
    end
  else
    echo No argument
  end
end
complete --command rc --require-parameter --no-files --arguments "vim neovim kakoune alacritty kitty chunkwm skhd hammerspoon fish zsh bash git ranger"

# update -d Run all update commands {{{2
function update -d "Run all update commands"
  if contains "all" $argv
    echo "-- Updating Fish command completions..."
    fish_update_completions

    if test -e /usr/local/bin/stack
      echo "-- Updating stack packages..."
      stack update
    end

    if test -e $HOME/.cargo/bin/rustup
      echo "-- Updating rustup..."
      rustup update
    end

    if test -e /usr/local/bin/npm
      echo "-- Updating npm packages..."
      npm update
    end
  end

  if test -e /usr/local/bin/nvim
    if test -d $HOME/.config/nvim/plugged
      echo "-- Updating Neovim plugins..."
      nvim +PlugUpgrade +PlugUpdate +qa
    end
  else
    if test -d $HOME/.vim/plugged
      echo "-- Updating Vim plugins..."
      vim +PlugUpgrade +PlugUpdate +qa
    end
  end

  echo "-- Updating system packages..."
  softwareupdate -lia

  if test -e /usr/local/bin/mas
    echo "-- Updating App Store apps..."
    mas outdated
    mas upgrade
  end

  if test -e /usr/local/bin/brew
    echo "-- Updating Homebrew packages..."
    brew update
    brew upgrade

    if contains "clean" $argv
      brew cleanup
      brew cask cleanup
    end
  end

  echo "-- Updates complete!"
end
complete --command update --require-parameter --no-files --arguments "all clean"


# runcpp - Run C++ file and then delete output {{{2
function runcpp -d "Run C++ file and then delete output"
  clang++ -Wall -std=c++14 -o fish_runcpp.temp $argv[1]; and ./fish_runcpp.temp; rm fish_runcpp.temp
end
complete --command runcpp --require-parameter

# }}}


# ABBREVIATIONS {{{1
if status --is-interactive
  set -g fish_user_abbreviations

  abbr --add cask   "brew cask"
  abbr --add ghc    "stack ghc"
  abbr --add ghci   "stack ghci"
  abbr --add runghc "stack runghc"
end


# PROMPT {{{1
set -g fish_greeting
set -g fish_prompt_pwd_dir_length 0
set __fish_git_prompt_showdirtystate "true"
set __fish_git_prompt_showuntrackedfiles "true"
set __fish_git_prompt_showstashstate "true"

# Helpers {{{
set -g current_bg NONE
set -g segment_separator \uE0B0

function prompt_segment -d "Function to draw a segment"
  set -l bg
  set -l fg
  if [ -n "$argv[1]" ]
    set bg $argv[1]
  else
    set bg normal
  end
  if [ -n "$argv[2]" ]
    set fg $argv[2]
  else
    set fg normal
  end
  if [ "$current_bg" != 'NONE' -a "$argv[1]" != "$current_bg" ]
    set_color -b $bg
    set_color $current_bg
    echo -n "$segment_separator "
    set_color -b $bg
    set_color $fg
  else
    set_color -b $bg
    set_color $fg
    echo -n " "
  end
  set current_bg $argv[1]
  if [ -n "$argv[3]" ]
    echo -n -s $argv[3] " "
  end
end

function prompt_finish -d "Close open segments"
  if [ -n $current_bg ]
    set_color -b normal
    set_color $current_bg
    echo -n "$segment_separator "
  end
  set_color normal
  set -g current_bg NONE
end
# }}}

function fish_prompt
  set -l exit_code $status
  set_color --background black white

  # Exit code
  if test $exit_code -ne 0
    prompt_segment red black $exit_code
  end

  # PWD
  prompt_segment white black (prompt_pwd)

  # Git
  if test -d .git -o -d ../.git -o (git rev-parse --git-dir > /dev/null 2>&1; echo $status) -eq 0
    set -l git_text (__fish_git_prompt | sed -n "s/.*(\(.*\)).*/\1/p")
    set -l git_bg black
    set -l git_fg white
    # switch (string sub --length=1 --start=-1 $git_text)
    #   case "+" "\*"
    #     set git_bg yellow
    #     set git_fg black
    #   case "%"
    #     set git_bg red
    #     set git_fg black
    # end
    prompt_segment $git_bg $git_fg (__fish_git_prompt | sed -n "s/.*(\(.*\)).*/\1/p")
  end

  prompt_finish
end


# COLORS {{{1
set fish_color_command         blue
set fish_color_param           normal
set fish_color_quote           green
set fish_color_error           red
set fish_color_valid_path      --underline
set fish_color_comment         brblack
set fish_color_autosuggestion  brblack

# iTERM {{{1
if test -e $HOME/.config/fish/iterm2_shell_integration.fish
  source $HOME/.config/fish/iterm2_shell_integration.fish
end



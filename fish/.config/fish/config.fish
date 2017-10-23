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
alias git "hub"
alias reload "source $HOME/.config/fish/config.fish"

switch (uname)
case Darwin
  alias rm "trash"
  alias tower "gittower"
  alias refresh "killall SystemUIServer; killall Dock; killall ControlStrip; pkill \"Touch Bar agent\""

  function iso2img -d "Convert an ISO to an IMG"
    for iso in $argv
      hdiutil convert -format UDRW -o "$iso.img" "$iso"
      mv "$iso.img.dmg" "$iso.img"
      mv "$iso.img" (echo "$iso.img" | sed "s/\.iso//")
    end
  end
  complete --command iso2img --require-parameter
case Linux
end

# update - Run all update commands {{{2
function update -d "Run all update commands"
  switch (uname)
  case Darwin
    set_color yellow; echo "-- Updating macOS software..."; set_color normal
    softwareupdate -lia
    test (which mas); and mas upgrade

    if test (which brew)
      set_color yellow; echo "-- Updating Homebrew packages..."; set_color normal
      brew update
      brew upgrade
    end
  case Linux
    set_color yellow; echo "-- Updating Arch Linux packages..."; set_color normal
    sudo pacman -Syu

    if test (which aura)
      set_color yellow; echo "-- Updating AUR packages..."; set_color normal
      sudo aura -Aua
    end
  end

  if test (which npm)
    set_color yellow; echo "-- Updating NPM packages..."; set_color normal
    npm update -g
  end

  if test (which stack)
    set_color yellow; echo "-- Updating Haskell Stack packages..."; set_color normal
    stack update
  end

  if test (which rustup)
    set_color yellow; echo "-- Updating Rust..."; set_color normal
    rustup update
  end

  if test (which nvim) -a -e $HOME/.config/nvim/autoload/plug.vim
    set_color yellow; echo "-- Updating Neovim packages..."; set_color normal
    nvim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
  else if test (which vim) -a -e $HOME/.vim/autoload/plug.vim
    set_color yellow; echo "-- Updating Vim packages..."; set_color normal
    vim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
  end

  set_color yellow; echo "-- Updating Fish command completions..."; set_color normal
  fish_update_completions
end

# rc - Open the specified program's configuration file {{{2
function rc -d "Open the specified program's configuration file"
  if test (count $argv) -gt 0
    switch $argv[1]
    # Editors
    case vim vi
      eval $EDITOR $HOME/.vimrc
    case neovim nvim
      eval $EDITOR $HOME/.config/nvim/init.vim

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
    case hammerspoon
      eval $EDITOR $HOME/.hammerspoon/init.lua

    case "*"
      echo Not defined: $argv[1]
  end
  else
    echo No argument
  end
end
complete --command rc --require-parameter --no-files --arguments "vim neovim kakoune alacritty kitty chunkwm skhd hammerspoon fish zsh bash git ranger"

# runcpp - Run C++ file and then delete output {{{2
function runcpp -d "Run C++ file and then delete output"
  clang++ -Wall -std=c++14 -o fish_runcpp.temp $argv[1]; and ./fish_runcpp.temp; rm fish_runcpp.temp
end
complete --command runcpp --require-parameter

# }}}


# ABBREVIATIONS {{{1
if status --is-interactive
  set -g fish_user_abbreviations

  switch (uname)
  case Darwin
    abbr --add cask "brew cask"
  case Linux
  case "*"
    abbr --add ghc "stack ghc"
    abbr --add ghci "stack ghci"
    abbr --add runghc "stack runghc"
  end
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



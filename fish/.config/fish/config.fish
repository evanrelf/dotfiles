# vim: foldmethod=marker foldenable

if test (command -s jump)
  status --is-interactive; and source (jump shell fish | psub)
end

# WORK {{{1
alias qa "~/qa.bash"
alias qaa "cat ~/qa.bash | vipe | bash"
alias vpn "~/vpn.bash"


# VARIABLES {{{1
set -x EDITOR nvim
set -x MANPAGER "nvim -c 'set ft=man' -"
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_FIND_FILE_COMMAND "ag -l --hidden --ignore .git"
set -U FZF_OPEN_COMMAND "$FZF_FIND_FILE_COMMAND"

set -l paths $HOME/.cargo/bin $HOME/.local/bin
for i in $paths
  if test -d $i
    set -x PATH $i $PATH
  end
end


# COMMANDS {{{1
alias reload "source $HOME/.config/fish/config.fish"

alias ed "pkill Emacs; pkill Emacs; emacs --daemon=term; emacs --daemon=gui"
alias et "emacsclient -s term -t"
alias eg "emacsclient -s gui -c -n"

alias wg-up "wg-quick up mullvad-us2"
alias wg-down "wg-quick down mullvad-us2"

alias pandochtml "pandoc -o output.html --to=html5 --css=\"$HOME/Docments/github.css\" --highlight-style=haddock --self-contained"

if test (command -s exa)
  alias ls "exa --group-directories-first"
  alias tree "exa --tree --group-directories-first"
else
  alias ls "ls -AFGh"
end

if test (command -s hub)
  alias git "hub"
end

switch (uname)
  case Linux
    # nothing

  case Darwin
    alias cask "brew cask"

    if test (command -s gittower)
      alias tower "gittower ."
    end

    if test -e /Applications/Marked\ 2.app
      alias marked "open -a Marked\ 2.app"
    end

    # refresh - Restart system applications {{{2
    function refresh -d "Restart system applications"
      defaults write com.apple.dock ResetLaunchPad -bool true 2>/dev/null; or echo "Failed to kill SystemUIServer" >&2
      killall SystemUIServer 2>/dev/null; or echo "Failed to kill SystemUIServer" >&2
      killall Dock 2>/dev/null; or echo "Failed to kill Dock" >&2
      killall Finder 2>/dev/null; or echo "Failed to kill Finder" >&2
      killall ControlStrip 2>/dev/null; or echo "Failed to kill ControlStrip" >&2
    end

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

    # pman - Open man page as PDF in Preview {{{2
    function pman -d "Open man page as PDF in Preview" -w man
      man -t $argv[1] | open -f -a Preview
    end

    # }}}2

  case '*'
    # nothing
end

# update - Run all update commands {{{2
function update -d "Run all update commands"
  switch (uname)
    case Linux
      if test (command -s pacman)
        # Arch Linux
        set_color yellow; echo "== Updating Arch Linux packages"; set_color normal
        sudo pacman -Syu
      else if test (command -s nix-env)
        # NixOS
        set_color yellow; echo "== Updating NixOS derivations"; set_color normal
        nix-channel --update
        nix-env --upgrade
        sudo nixos-rebuild switch --upgrade
      else if test (command -s apt)
        # Ubuntu & Debian
        set_color yellow; echo "== Updating Ubuntu/Debian packages"; set_color normal
        sudo apt update
        sudo apt upgrade
      else if test (command -s dnf)
        # Fedora & Red Hat
        set_color yellow; echo "== Updating Fedora/Red Hat packages"; set_color normal
        sudo dnf upgrade
      end
    case Darwin
      set_color yellow; echo "== Updating macOS software"; set_color normal
      softwareupdate -lia
      test (command -s mas); and mas upgrade

      if test (command -s brew)
        set_color yellow; echo "== Updating Homebrew packages"; set_color normal
        brew update
        brew upgrade
      end
    case '*'
      # nothing
  end

  if test (command -s npm)
    set_color yellow; echo "== Updating NPM packages"; set_color normal
    npm update -g
  end

  if test (command -s stack)
    set_color yellow; echo "== Updating Haskell Stack packages"; set_color normal
    stack update
  end

  if test (command -s rustup)
    set_color yellow; echo "== Updating Rust"; set_color normal
    rustup update
  end

  if test (command -s nvim) -a -e $HOME/.config/nvim/autoload/plug.vim
    set_color yellow; echo "== Updating Neovim packages"; set_color normal
    nvim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
  else if test (command -s vim) -a -e $HOME/.vim/autoload/plug.vim
    set_color yellow; echo "== Updating Vim packages"; set_color normal
    vim +PlugClean! +PlugUpgrade +"PlugUpdate --sync" +qa
  end

  if test (command -s tldr)
    set_color yellow; echo "== Updating tldr"; set_color normal
    tldr --update
  end

  set_color yellow; echo "== Updating Fish command completions"; set_color normal
  fish_update_completions
end

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
      eval $EDITOR "$HOME/.config/kak/kakrc"
    case emacs
      eval $EDITOR "$HOME/.emacs"
    case spacemacs
      eval $EDITOR "$HOME/.spacemacs"
    case doom
      eval $EDITOR "$HOME/.emacs.d/init.el"
    case vscode
      eval $EDITOR "$HOME/Library/Application\ Support/Code/User/settings.json"

    # Shells
    case fish
      eval $EDITOR "$HOME/.config/fish/config.fish"
    case zsh
      eval $EDITOR "$HOME/.zshrc"
    case bash
      eval $EDITOR "$HOME/.bashrc"

    # Window managers
    case bspwm
      eval $EDITOR "$HOME/.config/bspwm/bspwmrc"
    case sxhkd
      eval $EDITOR "$HOME/.config/sxhkd/sxhkdrc"
    case xmonad
      eval $EDITOR "$HOME/.config/xmonad/xmonad.hs"

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
    case hammerspoon
      eval $EDITOR "$HOME/.hammerspoon/init.lua"
    case alacritty
      eval $EDITOR "$HOME/.config/alacritty/alacritty.yml"
    case kitty
      eval $EDITOR "$HOME/.config/kitty/kitty.conf"
    case nixos
      eval sudoedit /etc/nixos/configuration.nix

    case "*"
      echo "Not config defined for '$argv[1]'" >&2
      return 1
  end
  else
    echo "No config specified" >&2
    return 1
  end
end
complete --command rc --require-parameter --no-files --arguments "vim neovim kakoune emacs spacemacs vscode fish zsh bash bspwm sxhkd xmonad xresources xinit tmux git hammerspoon alacritty kitty nixos"

# runcpp - Run C++ file and then delete output {{{2
function runcpp -d "Run C++ file and then delete output" -w clang++
  if test (command -s clang++)
    echo "Compiling..."
    clang++ -Wall -std=c++14 -o fish_runcpp.temp $argv[1]
    and echo "Running..."
    and ./fish_runcpp.temp
    rm fish_runcpp.temp
  else if test (command -s g++)
    echo "Compiling..."
    g++ -Wall -std=c++14 -o fish_runcpp.temp $argv[1]
    and echo "Running..."
    and ./fish_runcpp.temp
    rm fish_runcpp.temp
  else
    echo "No C++ compiler installed" >&2
    return 1
  end
end

# }}}

if status --is-interactive
  set -g fish_user_abbreviations
  abbr --add ghc "stack ghc"
  abbr --add ghci "stack ghci"
  abbr --add runghc "stack runghc"
end

function echo-replace
  echo -ne "\e[0K\r$argv"
end


# PROMPT {{{1
set -g fish_greeting ""
set -g fish_prompt_pwd_dir_length 0
set __fish_git_prompt_showdirtystate "true"
set __fish_git_prompt_showuntrackedfiles "true"
set __fish_git_prompt_showstashstate "true"

# Helpers {{{
set -g current_bg NONE
# set -g segment_separator \uE0B0
set -g segment_separator ""

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
  # echo ""

  # PWD
  set_color cyan
  echo -n (prompt_pwd)" "
  set_color normal

  # Git
  set -l git_dir (git rev-parse --git-dir 2> /dev/null)
  if test -n "$git_dir"
    set -l dirty (git status --porcelain)
    if test -n "$dirty"
      set_color yellow
    else
      set_color green
    end
    echo -n (git symbolic-ref --short HEAD 2>/dev/null)" "
    set_color normal
  end

  # Exit status
  if test $exit_code -ne 0
    set_color red
  end
  echo -n "λ "
  set_color normal
end


# COLORS {{{1
set fish_color_command blue
set fish_color_param normal
set fish_color_quote green
set fish_color_error red
set fish_color_valid_path --underline
set fish_color_comment brblack
set fish_color_autosuggestion brblack


# iTERM {{{1
if test ! -e $HOME/.config/fish/iterm2_shell_integration.fish
  curl -o $HOME/.config/fish/iterm2_shell_integration.fish https://iterm2.com/shell_integration/fish
end
source $HOME/.config/fish/iterm2_shell_integration.fish


# NIX {{{1
if test -e $HOME/.nix-profile/etc/profile.d/nix.sh
  bass source $HOME/.nix-profile/etc/profile.d/nix.sh
end


#}}}1

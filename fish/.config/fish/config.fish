# vim: foldmethod=marker foldenable

# VARIABLES {{{1
set -x EDITOR nvim
# set -x MANPAGER "nvim -c 'set ft=man' -"
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

if test (command -s exa)
  alias ls "exa --group-directories-first"
  alias tree "exa --tree --group-directories-first"
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

if status --is-interactive
  if test (command -s stack)
    set -g fish_user_abbreviations
    abbr --add ghc "stack ghc"
    abbr --add ghci "stack ghci"
    abbr --add runghc "stack runghc"
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
  set -l start (pwd)
  while test ! -d .git -a (pwd) != "$HOME"
    cd ..
  end
  if test (pwd) = "$HOME"
    cd $start
    echo "Not in Git repository"
    return 1
  end
  return 0
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
# }}}2
# refresh - Restart system applications {{{2
function refresh -d "Restart system applications"
  defaults write com.apple.dock ResetLaunchPad -bool true 2>/dev/null; or echo "Failed to kill SystemUIServer" >&2
  killall SystemUIServer 2>/dev/null; or echo "Failed to kill SystemUIServer" >&2
  killall Dock 2>/dev/null; or echo "Failed to kill Dock" >&2
  killall Finder 2>/dev/null; or echo "Failed to kill Finder" >&2
  killall ControlStrip 2>/dev/null; or echo "Failed to kill ControlStrip" >&2
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

# PROMPT {{{1
set -g fish_greeting ""
set -g fish_prompt_pwd_dir_length 0
set __fish_git_prompt_showdirtystate "true"
set __fish_git_prompt_showuntrackedfiles "true"
set __fish_git_prompt_showstashstate "true"

function fish_prompt
  set -l exit_code $status
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
  # Prompt character
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


# EXTRAS {{{1
# Auto-install fisher and plugins {{{2
if not functions -q fisher
  echo "Installing fisher for the first time..." >&2
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  source $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fisher
end

# iTerm shell integration {{{2
if test -e $HOME/.config/fish/iterm2_shell_integration.fish
  source $HOME/.config/fish/iterm2_shell_integration.fish
end
# curl -o $HOME/.config/fish/iterm2_shell_integration.fish https://iterm2.com/shell_integration/fish

# Nix {{{2
if test -e $HOME/.nix-profile/etc/profile.d/nix.sh
  bass source $HOME/.nix-profile/etc/profile.d/nix.sh
end
# }}}2


# }}}1

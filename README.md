# dotfiles

My personal dotfiles

## Overview

- Editor: [Kakoune][kakoune]
- Shell: [fish][fish]
- Terminal: [kitty][kitty]
- Window Manager: [tmux][tmux] in my terminal, [Hammerspoon][hammerspoon] on
  macOS, [sway][sway] on NixOS
- Package Manager: [Nix][nix], [Homebrew][homebrew] (for GUI apps on macOS)
- OS: [macOS][macos], [NixOS][nixos]

Dotfiles are managed using a combination of [GNU Stow][gnustow],
`{before,after}-hook` shell scripts, and [my install script](./install.hs)
written in Haskell.

[kakoune]: https://github.com/mawww/kakoune
[fish]: https://fishshell.com/
[kitty]: https://sw.kovidgoyal.net/kitty/
[tmux]: https://github.com/tmux/tmux
[hammerspoon]: https://www.hammerspoon.org/
[sway]: https://github.com/swaywm/sway
[nix]: https://nixos.org/
[homebrew]: https://brew.sh/
[macos]: https://www.apple.com/macos/
[nixos]: https://nixos.org/
[gnustow]: https://www.gnu.org/software/stow/

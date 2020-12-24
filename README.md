# dotfiles

My personal dotfiles

## Overview

- Editor: [Kakoune][kakoune]
- Shell: [fish][fish]
- Terminal: [kitty][kitty]
- Window Manager: [tmux][tmux] in my terminal, [Amethyst][amethyst] on macOS,
  [XMonad][xmonad] on NixOS
- Package Manager: [Nix][nix], [Homebrew][homebrew] (for GUI apps on macOS)
- OS: [macOS][macos], [NixOS][nixos]

Dotfiles are managed using [GNU Stow][gnustow], which is run via [my install
script](./install.hs).

## Usage

You must have [Nix][nix] installed to run the install script.

```shell
$ ./install.hs [--dry-run] PACKAGE...
```

[kakoune]: https://github.com/mawww/kakoune
[fish]: https://fishshell.com/
[kitty]: https://sw.kovidgoyal.net/kitty/
[amethyst]: https://github.com/ianyh/Amethyst
[xmonad]: https://xmonad.org/
[tmux]: https://github.com/tmux/tmux
[nix]: https://nixos.org/
[homebrew]: https://brew.sh/
[nixos]: https://nixos.org/
[macos]: https://www.apple.com/macos/
[gnustow]: https://www.gnu.org/software/stow/

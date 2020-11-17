# dotfiles

My personal dotfiles

## Overview

- Editor: [Kakoune][kakoune], [Doom Emacs][doom-emacs]
- Shell: [fish][fish]
- Terminal: [kitty][kitty]
- Window Manager: [tmux][tmux] in my terminal, [Amethyst][amethyst] on macOS, [XMonad][xmonad] on NixOS
- Package Manager: [Nix][nix], [Homebrew](homebrew) (for GUI apps on macOS)
- OS: [macOS][macos], [NixOS][nixos]

## Usage

You must have [Nix][nix] installed to run the install script.

```shell
$ ./install.hs PACKAGES [--dry-run]
```

[kakoune]: https://github.com/mawww/kakoune
[doom-emacs]: https://github.com/hlissner/doom-emacs
[fish]: https://fishshell.com/
[kitty]: https://sw.kovidgoyal.net/kitty/
[amethyst]: https://github.com/ianyh/Amethyst
[xmonad]: https://xmonad.org/
[tmux]: https://github.com/tmux/tmux
[nix]: https://nixos.org/
[homebrew]: https://brew.sh/
[nixos]: https://nixos.org/
[macos]: https://www.apple.com/macos/

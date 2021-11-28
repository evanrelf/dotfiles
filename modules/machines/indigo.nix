{ pkgs, ... }:

{
  imports = [
    ./awake.nix
    ./common.nix
  ];

  dotfiles.programs = {
    hammerspoon.enable = true;
    karabiner.enable = true;
    kitty.enable = true;
  };

  home.packages = [ pkgs.tmux-xpanes ];
}

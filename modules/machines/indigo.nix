{ pkgs, ... }:

{
  imports = [
    ../programs/hammerspoon.nix
    ../programs/karabiner.nix
    ../programs/kitty.nix
    ./awake.nix
    ./common.nix
  ];

  home.packages = [ pkgs.tmux-xpanes ];
}

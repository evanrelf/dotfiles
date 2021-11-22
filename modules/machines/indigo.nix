{ pkgs, ... }:

{
  imports = [
    ../programs/kitty.nix
    ./awake.nix
    ./common.nix
  ];

  home.packages = [ pkgs.tmux-xpanes ];
}

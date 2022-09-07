{ config, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/Users/${config.home.username}";

  home.packages = [
    pkgs.bashInteractive
    pkgs.fnm
    pkgs.gnugrep
    pkgs.yarn
  ];
}

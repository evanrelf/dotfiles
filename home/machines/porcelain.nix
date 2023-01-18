{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/Users/${config.home.username}";

  home.packages = [
    pkgs.awscli2
  ];
}

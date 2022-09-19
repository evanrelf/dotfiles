{ config, ... }:

{
  imports = [
    ./common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/home/${config.home.username}";
}

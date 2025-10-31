{ config, ... }:

{
  imports = [
    ../common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/Users/${config.home.username}";
}

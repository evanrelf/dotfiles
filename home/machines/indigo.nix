{ config, ... }:

{
  imports = [
    ./common.nix
  ];

  home.username = "evan";

  home.homeDirectory = "/Users/${config.home.username}";
}

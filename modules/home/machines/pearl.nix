{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  home.stateVersion = "22.11";
}

{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  home.stateVersion = "25.11";
}

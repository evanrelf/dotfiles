{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  home.packages = with pkgs; [
    # TODO: Move to common Darwin packages once `pearl` upgrades to macOS Tahoe.
    container
  ];

  home.stateVersion = "22.11";
}

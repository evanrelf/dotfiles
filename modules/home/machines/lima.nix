{ config, lib, ... }:

{
  imports = [
    ../common.nix
  ];

  home.homeDirectory = lib.mkForce "/home/${config.home.username}.linux";

  home.file."Code/${config.home.username}/dotfiles".source =
    config.lib.file.mkOutOfStoreSymlink "/Users/${config.home.username}/Code/${config.home.username}/dotfiles";

  home.stateVersion = "25.11";
}

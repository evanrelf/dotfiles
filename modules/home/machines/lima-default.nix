{ config, ... }:

{
  imports = [
    ../common.nix
  ];

  home.username = "evanrelf";

  home.homeDirectory = "/home/${config.home.username}.linux";

  home.file."Code/evanrelf/dotfiles".source =
    config.lib.file.mkOutOfStoreSymlink "/Users/evanrelf/Code/evanrelf/dotfiles";
}

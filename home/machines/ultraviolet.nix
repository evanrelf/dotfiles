{ pkgs, ... }:

{
  home.packages = with pkgs; [
    home-manager
    neovim
    zsh
  ];

  home.file.".zshrc".source = ../../configs/zsh/.zshrc;
}

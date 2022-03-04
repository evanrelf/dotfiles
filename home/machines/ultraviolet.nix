{ pkgs, ... }:

{
  home.packages = with pkgs; [
    git
    home-manager
    neovim
    zsh
  ];

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
    recursive = true;
  };

  home.file.".zshrc".source = ../../configs/zsh/.zshrc;
}

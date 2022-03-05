{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fish
    git
    home-manager
    neovim
  ];

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
    recursive = true;
  };

  xdg.configFile."kitty" = {
    source = ../../configs/kitty/.config/kitty;
    recursive = true;
  };
}

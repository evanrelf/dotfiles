{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fd
    fish
    fzf
    git
    home-manager
    neovim
    ripgrep
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

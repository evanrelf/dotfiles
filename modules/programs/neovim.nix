{ inputs, lib, pkgs, ... }:

{
  home.packages = [ pkgs.neovim ];

  xdg.configFile."nvim" = {
    source = ../../configs/neovim/.config/nvim;
    recursive = true;
  };

  xdg.dataFile."nvim/site/autoload/plug.vim".source =
    "${inputs.vim-plug}/plug.vim";
}

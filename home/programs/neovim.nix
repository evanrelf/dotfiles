{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.neovim;

in
{
  options = {
    dotfiles.programs.neovim = {
      enable = lib.mkEnableOption "neovim";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.neovim ];

    xdg.configFile."nvim" = {
      source = ../../configs/neovim/.config/nvim;
      recursive = true;
    };

    xdg.dataFile."nvim/site/autoload/plug.vim".source =
      "${inputs.vim-plug}/plug.vim";
  };
}

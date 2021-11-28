{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.alacritty;

in
{
  options = {
    dotfiles.programs.alacritty = {
      enable = lib.mkEnableOption "alacritty";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.mkIf pkgs.stdenv.isLinux [ pkgs.alacritty ];

    xdg.configFile."alacritty.yml".source =
      ../../configs/alacritty/.config/alacritty.yml;
  };
}

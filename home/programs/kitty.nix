{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.kitty;

in
{
  options = {
    dotfiles.programs.kitty = {
      enable = lib.mkEnableOption "kitty";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.mkIf pkgs.stdenv.isLinux [ pkgs.kitty ];

    xdg.configFile."kitty/kitty.conf".source =
      ../../configs/kitty/.config/kitty/kitty.conf;
  };
}

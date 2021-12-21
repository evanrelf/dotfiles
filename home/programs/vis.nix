{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.vis;

in
{
  options = {
    dotfiles.programs.vis = {
      enable = lib.mkEnableOption "vis";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.vis ];

    xdg.configFile."vis/visrc.lua".source =
      ../../configs/wezterm/.config/vis/visrc.lua;
  };
}

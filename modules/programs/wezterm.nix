{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.wezterm;

in
{
  options = {
    dotfiles.programs.wezterm = {
      enable = lib.mkEnableOption "wezterm";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."wezterm/wezterm.lua".source =
      ../../configs/wezterm/.config/wezterm/wezterm.lua;
  };
}

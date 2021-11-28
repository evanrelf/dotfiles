{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.yabai;

in
{
  options = {
    dotfiles.programs.yabai = {
      enable = lib.mkEnableOption "yabai";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."yabai/yabairc".source =
      ../../configs/yabai/.config/yabai/yabairc;
  };
}

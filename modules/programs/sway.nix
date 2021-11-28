{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.sway;

in
{
  options = {
    dotfiles.programs.sway = {
      enable = lib.mkEnableOption "sway";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."sway" = {
      source = ../../configs/sway/.config/sway;
      recursive = true;
    };
  };
}

{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.skhd;

in
{
  options = {
    dotfiles.programs.skhd = {
      enable = lib.mkEnableOption "skhd";
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."skhd/skhdrc".source =
      ../../configs/skhd/.config/skhd/skhdrc;
  };
}

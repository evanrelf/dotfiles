{ config, lib, ... }:

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
    xdg.configFile."alacritty.yml".source =
      ../../configs/alacritty/.config/alacritty.yml;
  };
}

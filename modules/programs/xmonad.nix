{ config, lib, ... }:

let
  cfg = config.dotfiles.programs.xmonad;

in
{
  options = {
    dotfiles.programs.xmonad = {
      enable = lib.mkEnableOption "xmonad";
    };
  };

  config = lib.mkIf cfg.enable {
    home.file.".xmonad" = {
      source = ../../configs/xmonad;
      recursive = true;
    };
  };
}

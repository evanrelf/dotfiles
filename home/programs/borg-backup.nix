{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.borg-backup;

in
{
  options = {
    dotfiles.programs.borg-backup = {
      enable = lib.mkEnableOption "borg-backup";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.borgbackup ];

    xdg.configFile."borg" = {
      source = ../../configs/borg/.config/borg;
      recursive = true;
    };
  };
}

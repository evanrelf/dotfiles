{ config, lib, pkgs, ... }:

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
    assertions = [
      {
        assertion = pkgs.stdenv.isLinux;
        message = "sway: Only works on Linux";
      }
    ];

    xdg.configFile."sway" = {
      source = ../../configs/sway/.config/sway;
      recursive = true;
    };
  };
}

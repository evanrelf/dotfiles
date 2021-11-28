{ config, lib, pkgs, ... }:

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
    assertions = [
      {
        assertion = pkgs.stdenv.isLinux;
        message = "skhd: Only works on Linux";
      }
    ];

    xdg.configFile."skhd/skhdrc".source =
      ../../configs/skhd/.config/skhd/skhdrc;
  };
}

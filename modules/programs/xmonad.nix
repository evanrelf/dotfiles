{ config, lib, pkgs, ... }:

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
    assertions = [
      {
        assertion = pkgs.stdenv.isLinux;
        message = "xmonad: Only works on Linux";
      }
    ];

    home.file.".xmonad" = {
      source = ../../configs/xmonad;
      recursive = true;
    };
  };
}

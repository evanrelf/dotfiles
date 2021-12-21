{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.helix;

in
{
  options = {
    dotfiles.programs.helix = {
      enable = lib.mkEnableOption "helix";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.helix ];

    xdg.configFile."helix" = {
      source = ../../configs/helix/.config/helix;
      recursive = true;
    };
  };
}

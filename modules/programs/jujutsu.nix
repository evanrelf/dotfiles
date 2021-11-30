{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.jujutsu;

in
{
  options = {
    dotfiles.programs.jujutsu = {
      enable = lib.mkEnableOption "jujutsu";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.jujutsu ];
  };
}

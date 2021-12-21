{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.starship;

in
{
  options = {
    dotfiles.programs.starship = {
      enable = lib.mkEnableOption "starship";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.starship ];

    xdg.configFile."starship.toml".source =
      ../../configs/starship/.config/starship.toml;
  };
}

{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.kakoune;

in
{
  options = {
    dotfiles.programs.kakoune = {
      enable = lib.mkEnableOption "kakoune";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.kakoune ];

    xdg.configFile."kak" = {
      source = ../../configs/kakoune/.config/kak;
      recursive = true;
    };

    xdg.configFile."kak/autoload/runtime".source =
      "${pkgs.kakoune}/share/kak/autoload";
  };
}

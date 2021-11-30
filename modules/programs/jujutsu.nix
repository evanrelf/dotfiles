{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.jujutsu;

in
{
  options = {
    dotfiles.programs.jujutsu = {
      enable = lib.mkEnableOption "jujutsu";

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.jujutsu ];

    home.file.".jjconfig".text =
      lib.mkIf (cfg.enable && cfg.config != null) cfg.config;
  };
}

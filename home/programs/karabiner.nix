{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.karabiner;

in
{
  options = {
    dotfiles.programs.karabiner = {
      enable = lib.mkEnableOption "karabiner";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "karabiner: Only works on macOS";
      }
    ];

    xdg.configFile."karabiner/assets/complex_modifications/evan.json".source =
      ../../configs/karabiner/.config/karabiner/assets/complex_modifications/evan.json;
  };
}
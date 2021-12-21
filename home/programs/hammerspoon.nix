{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.hammerspoon;

in
{
  options = {
    dotfiles.programs.hammerspoon = {
      enable = lib.mkEnableOption "hammerspoon";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "hammerspoon: Only works on macOS";
      }
    ];

    xdg.configFile."hammerspoon/init.lua".source =
      ../../configs/hammerspoon/.config/hammerspoon/init.lua;

    home.activation.hammerspoonConfigLocation =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD defaults write org.hammerspoon.Hammerspoon \
          MJConfigFile "$HOME/.config/hammerspoon/init.lua"
      '';
  };
}

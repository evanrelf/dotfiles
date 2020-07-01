{ config, lib, pkgs, ... }:

let
  cfg = config.layers.hammerspoon;

in
  { options = {
      layers.hammerspoon = {
        enable = lib.mkEnableOption "hammerspoon layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.file."hammerspoon" = {
        source = ./files;
        target = ".";
        recursive = true;
      };

      home.activation."hammerspoon" =
        config.lib.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD defaults write \
            org.hammerspoon.Hammerspoon \
            MJConfigFile \
            "$HOME/.config/hammerspoon/init.lua"
        '';
    };
  }

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
    };
  }

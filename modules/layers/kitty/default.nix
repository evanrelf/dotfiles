{ config, lib, pkgs, ... }:

let
  cfg = config.layers.kitty;

in
  { options = {
      layers.kitty = {
        enable = lib.mkEnableOption "kitty layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.file."kitty" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

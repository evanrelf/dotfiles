{ config, lib, pkgs, ... }:

let
  cfg = config.layers.borg;

in
  { options = {
      layers.borg = {
        enable = lib.mkEnableOption "borg layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [ borgbackup ];

      home.file."borg" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

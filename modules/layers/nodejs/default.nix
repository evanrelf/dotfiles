{ config, lib, pkgs, ... }:

let
  cfg = config.layers.nodejs;

in
  { options = {
      layers.nodejs = {
        enable = lib.mkEnableOption "nodejs layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [ nodejs ];

      home.file."nodejs" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

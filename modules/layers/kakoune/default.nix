{ config, lib, pkgs, ... }:

let
  cfg = config.layers.kakoune;

in
  { options = {
      layers.kakoune = {
        enable = lib.mkEnableOption "kakoune layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [ kakoune ];

      home.file."kakoune" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

{ config, lib, pkgs, ... }:

let
  cfg = config.layers.fish;

in
  { options = {
      layers.fish = {
        enable = lib.mkEnableOption "fish layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [ fish ];

      home.file."fish" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

{ config, lib, pkgs, ... }:

let
  cfg = config.layers.karabiner;

in
  { options = {
      layers.karabiner = {
        enable = lib.mkEnableOption "karabiner layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.file."karabiner" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

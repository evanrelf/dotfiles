
{ config, lib, pkgs, ... }:

let
  cfg = config.layers.git;

in
  { options = {
      layers.git = {
        enable = lib.mkEnableOption "git layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        git
        gitAndTools.delta
        gitAndTools.hub
      ];

      home.file."git" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

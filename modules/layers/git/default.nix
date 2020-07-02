
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
      programs.git = {
        enable = true;

        userName = "Evan Relf";

        aliases = import ./aliases.nix;

        ignores = import ./ignores.nix;

        attributes = import ./attributes.nix;

        extraConfig = import ./extra-config.nix;

        includes = [ { path = "~/.config/git/local"; } ];
      };

      home.packages = with pkgs; [
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

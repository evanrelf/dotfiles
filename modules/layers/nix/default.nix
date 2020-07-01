{ config, lib, pkgs, ... }:

let
  cfg = config.layers.nix;

in
  { options = {
      layers.nix = {
        enable = lib.mkEnableOption "nix layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        cachix
        comma
        lorri
        nix
        nix-diff
        nix-prefetch-git
      ];

      home.file."nix" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

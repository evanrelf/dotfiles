{ config, lib, pkgs, ... }:

let
  cfg = config.layers.tmux;

in
  { options = {
      layers.tmux = {
        enable = lib.mkEnableOption "tmux layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [ tmux ];

      home.file."tmux" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

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

      home.activation."tmux" = config.lib.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d "$HOME/.config/tmux/plugins/tpm" ]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone \
            --depth 1 \
            "https://github.com/tmux-plugins/tpm.git" \
            "$HOME/.config/tmux/plugins/tpm"
        fi
      '';
    };
  }

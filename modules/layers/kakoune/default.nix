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

      home.activation."kakoune" =
        config.lib.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -d "$HOME/.config/kak/plugins/plug.kak" ]; then
            $DRY_RUN_CMD ${pkgs.git}/bin/git clone \
              --depth 1 \
              "https://github.com/andreyorst/plug.kak.git" \
              "$HOME/.config/kak/plugins/plug.kak"
          fi
        '';
    };
  }

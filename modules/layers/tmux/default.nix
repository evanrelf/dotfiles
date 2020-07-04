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
      programs.tmux = {
        enable = true;

        sensibleOnTop = false;

        terminal = "xterm-256color";

        baseIndex = 1;

        keyMode = "vi";

        newSession = true;

        shortcut = "s";

        aggressiveResize = true;

        escapeTime = 0;

        historyLimit = 5000;

        extraConfig = builtins.readFile ./tmux.conf;
      };

      home.activation."tmux" = config.lib.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d "$HOME/.config/tmux/plugins/tpm" ]; then
          echo "Downloading tmux plugin manager"
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone \
            --depth 1 \
            "https://github.com/tmux-plugins/tpm.git" \
            "$HOME/.config/tmux/plugins/tpm"
        fi
      '';
    };
  }

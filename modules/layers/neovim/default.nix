{ config, lib, pkgs, ... }:

let
  cfg = config.layers.neovim;

in
  { options = {
      layers.neovim = {
        enable = lib.mkEnableOption "neovim layer";
      };
    };

    config = lib.mkIf cfg.enable {
      programs.neovim = {
        enable = true;
        extraConfig = builtins.readFile ./init.vim;
      };

      home.activation."neovim" =
        config.lib.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -f "$HOME/.local/share/nvim/site/autoload/plug.vim" ]; then
            echo "Downloading plug.vim"
            $DRY_RUN_CMD ${pkgs.curl}/bin/curl \
              --location \
              --fail \
              --create-dirs \
              "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" \
              --output "$HOME/.local/share/nvim/site/autoload/plug.vim"
          fi
        '';
    };
  }

{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.neovim;

in
{
  options = {
    dotfiles.programs.neovim = {
      enable = lib.mkEnableOption "neovim";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.neovim ];

    xdg.configFile."nvim" = {
      source = ../../configs/neovim/.config/nvim;
      recursive = true;
    };

    xdg.dataFile."nvim/site/autoload/plug.vim".source =
      "${inputs.vim-plug}/plug.vim";

    home.activation.neovimInstallPlugins =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d "$HOME/.local/share/nvim/plugged" ]; then
          echo "Installing plugins"
          $DRY_RUN_CMD nvim \
            -u "$HOME/.config/nvim/lua/evan/plug.lua" \
            -i NONE \
            -c "PlugUpdate" -c "PlugClean!" -c "qa"
        fi
      '';
  };
}

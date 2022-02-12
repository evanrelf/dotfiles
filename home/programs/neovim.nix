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

    home.activation.neovimInstallPacker =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e "$HOME/.local/share/nvim/site/pack/packer/start/packer.nvim" ]; then
          $DRY_RUN_CMD git clone --depth 1 \
            "https://github.com/wbthomason/packer.nvim" \
            "$HOME/.local/share/nvim/site/pack/packer/start/packer.nvim"
        fi
      '';
  };
}

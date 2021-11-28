{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.fish;

in
{
  options = {
    dotfiles.programs.fish = {
      enable = lib.mkEnableOption "fish";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.fish ];

    xdg.configFile."fish" = {
      source = ../../configs/fish/.config/fish;
      recursive = true;
    };

    home.activation.installFisher =
      lib.hm.dag.entryAfter [ "writeBarrier" ] ''
        if [ ! -f "$HOME/.config/fish/functions/fisher.fish" ]; then
          echo "Downloading Fisher"
          $DRY_RUN_CMD curl \
            --location "https://git.io/fisher" \
            --output "$HOME/.config/fish/functions/fisher.fish" \
            --create-dirs

          echo "Installing plugins"
          $DRY_RUN_CMD fish -c "fisher update"
        fi
      '';
  };
}

{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.emacs;

in
{
  options = {
    dotfiles.programs.emacs = {
      enable = lib.mkEnableOption "emacs";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.dotfiles.programs.doom-emacs.enable;
        message = "emacs: dotfiles.programs.{doom-emacs,emacs} cannot be enabled simultaneously";
      }
    ];

    home.packages = [ pkgs.emacsCustom ];

    xdg.configFile."emacs".source = ../../configs/emacs/.config/emacs;

    home.file.".local/bin/evil".source = ../../configs/emacs/.local/bin/evil;

    home.activation.emacsTruecolor =
      lib.hm.dag.entryAfter [ "writeBoundary" "linkGeneration" ] ''
        if [ ! -e "$HOME/.local/share/terminfo" ]; then
          $DRY_RUN_CMD "$HOME/.config/emacs/setup-truecolor"
        fi
      '';
  };
}

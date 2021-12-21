{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.doom-emacs;

in
{
  options = {
    dotfiles.programs.doom-emacs = {
      enable = lib.mkEnableOption "doom-emacs";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.dotfiles.programs.emacs.enable;
        message = "doom-emacs: dotfiles.programs.{doom-emacs,emacs} cannot be enabled simultaneously";
      }
    ];

    home.packages = [ pkgs.emacsCustom ];

    xdg.configFile."doom" = {
      source = ../../configs/doom-emacs/.config/doom;
      recursive = true;
      onChange = ''
        echo "+ doom sync"
        $DRY_RUN_CMD "$HOME"/.config/emacs/bin/doom sync
      '';
    };

    home.file.".local/bin" = {
      source = ../../configs/emacs/.local/bin;
      recursive = true;
    };

    home.activation.emacsTruecolor =
      lib.hm.dag.entryAfter [ "writeBoundary" "linkGeneration" ] ''
        if [ ! -e "$HOME/.local/share/terminfo" ]; then
          $DRY_RUN_CMD emacs-setup-truecolor"
        fi
      '';

    home.activation.installDoomEmacs =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e "$HOME/.config/emacs" ]; then
          $DRY_RUN_CMD cp -r "${inputs.doom-emacs}" "$HOME/.config/emacs"
          $DRY_RUN_CMD chmod -R u+w "$HOME/.config/emacs"
        fi
      '';
  };
}

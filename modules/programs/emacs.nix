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

    home.file.".local/bin" = {
      source = ../../configs/emacs/.local/bin;
      recursive = true;
    };

    # xdg.configFile."emacs" = {
    #   source = ../../configs/emacs/.config/emacs;
    #   recursive = true;
    # };

    home.activation.emacsLinkConfig =
      lib.hm.dag.entryAfter [ "writeBoundary" "linkGeneration" ] ''
        if [ ! -e "$HOME/.config/emacs/init.el" ]; then
          $DRY_RUN_CMD mkdir -p "$HOME/.config/emacs"
          $DRY_RUN_CMD ln -s {$OLDPWD/configs/emacs,$HOME}/.config/emacs/init.el
        fi
      '';

    home.activation.emacsCleanupImperativePackages =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD rm -rf "$HOME"/.config/emacs/{eln-cache,elpa}
      '';

    home.activation.emacsTruecolor =
      lib.hm.dag.entryAfter [ "writeBoundary" "linkGeneration" ] ''
        if [ ! -e "$HOME/.local/share/terminfo" ]; then
          $DRY_RUN_CMD emacs-setup-truecolor
        fi
      '';
  };
}

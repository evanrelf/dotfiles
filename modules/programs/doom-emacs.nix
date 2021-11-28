{ config, lib, pkgs, ... }:

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
    };

    home.file.".local/bin/evil".source = ../../configs/emacs/.local/bin/evil;

    home.activation.emacsTruecolor =
      lib.hm.dag.entryAfter [ "writeBarrier" ] ''
        if [ ! -e "$HOME/.local/share/terminfo" ]; then
          $DRY_RUN_CMD ${../../configs/emacs/.config/emacs/setup-truecolor}
        fi
      '';

    home.activation.installDoomEmacs =
      lib.hm.dag.entryAfter [ "writeBarrier" ] ''
        if [ ! -e "$HOME/.config/emacs" ]; then
          $DRY_RUN_CMD git clone \
            --depth 1 \
            "https://github.com/hlissner/doom-emacs" \
            "$HOME/.config/emacs"
          $DRY_RUN_CMD "$HOME/.config/emacs/bin/doom" install
        fi
      '';
  };
}

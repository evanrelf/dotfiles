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
        assertion = !config.dotfiles.programs.doom.enable;
        message = "emacs: dotfiles.programs.{doom,emacs} cannot be enabled simultaneously";
      }
    ];

    home.packages = [ pkgs.emacsCustom ];

    xdg.configFile."emacs" = {
      source = ../../configs/emacs/.config/emacs;
      recursive = true;
    };

    home.file.".local/bin/evil".source = ../../configs/emacs/.local/bin/evil;

    home.activation.emacsTruecolor =
      lib.hm.dag.entryAfter [ "writeBarrier" ] ''
        if [ ! -e "$HOME/.local/share/terminfo" ]; then
          $DRY_RUN_CMD ${../../configs/emacs/.config/emacs/setup-truecolor}
        fi
      '';
  };
}

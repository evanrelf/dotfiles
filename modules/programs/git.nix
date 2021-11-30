{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.git;

in
{
  options = {
    dotfiles.programs.git = {
      enable = lib.mkEnableOption "git";

      email = lib.mkOption {
        description = "Email used for commits";
        type = lib.types.str;
      };

      signingkey = lib.mkOption {
        description = "GPG key used for signing commits";
        type = lib.types.str;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      delta
      git-branchless
      git-revise
      gitAndTools.gitFull
    ];

    xdg.configFile."git" = {
      source = ../../configs/git/.config/git;
      recursive = true;
    };

    xdg.configFile."git/local".text = ''
      [user]
        email = "${cfg.email}"
        signingkey = "${cfg.signingkey}"
    '';
  };
}

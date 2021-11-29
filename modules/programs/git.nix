{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.git;

in
{
  options = {
    dotfiles.programs.git = {
      enable = lib.mkEnableOption "git";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      delta
      gh
      git-branchless
      git-revise
      gitAndTools.gitFull
      perlPackages.GitAutofixup
    ];

    xdg.configFile."git" = {
      source = ../../configs/git/.config/git;
      recursive = true;
    };
  };
}

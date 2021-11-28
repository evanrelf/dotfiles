{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.mercurial;

in
{
  options = {
    dotfiles.programs.mercurial = {
      enable = lib.mkEnableOption "mercurial";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.mercurial ];

    home.file.".hgrc".source = ../../configs/mercurial/.hgrc;
  };
}

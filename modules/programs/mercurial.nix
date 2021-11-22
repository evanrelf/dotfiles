{ pkgs, ... }:

{
  home.packages = [ pkgs.mercurial ];

  home.file.".hgrc".source = ../../configs/mercurial/.hgrc;
}

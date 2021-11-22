{ pkgs, ... }:

{
  home.packages = [ pkgs.helix ];

  xdg.configFile."helix" = {
    source = ../../configs/helix/.config/helix;
    recursive = true;
  };
}

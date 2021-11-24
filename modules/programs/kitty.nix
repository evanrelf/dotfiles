{ lib, pkgs, ... }:

{
  home.packages = lib.mkIf pkgs.stdenv.isLinux [ pkgs.kitty ];

  xdg.configFile."kitty/kitty.conf".source =
    ../../configs/kitty/.config/kitty/kitty.conf;
}

{ pkgs, ... }:

{
  home.packages = [ pkgs.vis ];

  xdg.configFile."vis/visrc.lua".source =
    ../../configs/wezterm/.config/vis/visrc.lua;
}

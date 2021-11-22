{ pkgs, ... }:

{
  home.packages = [ pkgs.starship ];

  xdg.configFile."starship.toml".source =
    ../../configs/starship/.config/starship.toml;
}

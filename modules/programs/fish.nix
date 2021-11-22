{ pkgs, ... }:

{
  home.packages = [ pkgs.fish ];

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  # TODO Install fisher and update plugins
}

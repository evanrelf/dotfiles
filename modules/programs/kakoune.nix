{ pkgs, ... }:

{
  home.packages = [ pkgs.kakoune ];

  xdg.configFile."kak" = {
    source = ../../configs/kakoune/.config/kak;
    recursive = true;
  };

  xdg.configFile."kak/autoload/runtime".source =
    "${pkgs.kakoune}/share/kak/autoload";
}

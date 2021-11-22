{ pkgs, ... }:

{
  home.packages [ pkgs.borgbackup ];

  xdg.configFile."borg" = {
    source = ../../configs/borg/.config/borg;
    recursive = true;
  };
}

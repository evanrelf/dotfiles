{ ... }:

{
  xdg.configFile."sway" = {
    source = ../../configs/sway/.config/sway;
    recursive = true;
  };
}

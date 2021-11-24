{ pkgs, ... }:

{
  imports = [
    ../programs/kitty.nix
    ./common.nix
    ./personal.nix
  ];

  home.packages = with pkgs; [
    acpi
    autocutsel
    bemenu
    dmenu # keep for `dmenu_path | bemenu`
    firefox
    noto-fonts
    redshift
    spotify
    xbanish
    xclip
  ];
}

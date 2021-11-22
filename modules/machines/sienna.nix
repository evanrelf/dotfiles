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
    kitty
    noto-fonts
    redshift
    spotify
    xbanish
    xclip
  ];
}

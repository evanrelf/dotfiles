{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./personal.nix
  ];

  dotfiles.programs.kitty.enable = true;

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

{ inputs, ... }:

{
  imports = [
    ./common.nix
    ./personal.nix
  ];

  dotfiles.programs.hammerspoon.enable = true;

  dotfiles.programs.karabiner.enable = true;

  dotfiles.programs.kitty.enable = true;

  dotfiles.programs.homebrew = {
    enable = true;
    cleanup = true;
    zap = true;
    casks = [
      "1password"
      "alfred"
      "audio-hijack"
      "backblaze"
      "bartender"
      "discord"
      "firefox"
      "gpg-suite-no-mail"
      "hammerspoon"
      "iina"
      "imageoptim"
      "istat-menus"
      "karabiner-elements"
      "kitty"
      "minecraft"
      "mullvadvpn"
      "soundsource"
      "spotify"
      "the-unarchiver"
      "transmission"
      "transmit"
    ];
  };

  nixpkgs.overlays = [
    (pkgsFinal: pkgsPrev: {
      inherit (pkgsFinal.cross.x86_64-darwin)
        ghcid
        nix-index
        ormolu
        watchexec
        ;
    })
  ];
}
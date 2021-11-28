{ inputs, ... }:

{
  imports = [
    ./common.nix
    ./personal.nix
  ];

  dotfiles.programs = {
    hammerspoon.enable = true;
    karabiner.enable = true;
    kitty.enable = true;
  };

  nixpkgs.overlays = [
    (pkgsFinal: pkgsPrev: {
      ghcid = pkgsFinal.x86_64-darwin.ghcid;
      nix-index = pkgsFinal.x86_64-darwin.nix-index;
      ormolu = pkgsFinal.x86_64-darwin.ormolu;
      watchexec = pkgsFinal.x86_64-darwin.watchexec;
    })
  ];
}

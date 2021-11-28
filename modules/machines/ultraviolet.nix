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
      ghcid = pkgsFinal.cross.x86_64-darwin.ghcid;
      nix-index = pkgsFinal.cross.x86_64-darwin.nix-index;
      ormolu = pkgsFinal.cross.x86_64-darwin.ormolu;
      watchexec = pkgsFinal.cross.x86_64-darwin.watchexec;
    })
  ];
}

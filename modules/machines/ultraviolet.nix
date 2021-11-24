{ inputs, ... }:

{
  imports = [
    ../programs/hammerspoon.nix
    ../programs/karabiner.nix
    ../programs/kitty.nix
    ./common.nix
    ./personal.nix
  ];

  nixpkgs.overlays = [
    (pkgsFinal: pkgsPrev: {
      ghcid = pkgsFinal.x86_64-darwin.ghcid;
      nix-index = pkgsFinal.x86_64-darwin.nix-index;
      ormolu = pkgsFinal.x86_64-darwin.ormolu;
      watchexec = pkgsFinal.x86_64-darwin.watchexec;
    })
  ];
}

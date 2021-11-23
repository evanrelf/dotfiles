{ inputs, pkgs, ... }:

{
  imports = [
    ../programs/hammerspoon.nix
    ../programs/karabiner.nix
    ../programs/kitty.nix
    ./common.nix
    ./personal.nix
  ];

  nixpkgs.overlays = [
    (pkgsFinal: pkgsPrev:
      let
        pkgs-x86_64 = import inputs.nixpkgs {
          system = "x86_64-darwin";
          inherit (pkgs) config overlays;
        };
      in
      {
        ghcid = pkgs-x86_64.ghcid;
        nix-index = pkgs-x86_64.nix-index;
        ormolu = pkgs-x86_64.ormolu;
        watchexec = pkgs-x86_64.watchexec;
      })
  ];
}

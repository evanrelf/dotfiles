{ pkgs ? import <nixpkgs> { } }:

let
  ghc =
    pkgs.haskellPackages.ghcWithPackages (h: with h; [
      base
      xmonad
      xmonad-contrib
    ]);

in
pkgs.mkShell {
  buildInputs = [ ghc ];
}

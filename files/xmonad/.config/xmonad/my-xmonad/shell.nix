{ pkgs ? import <nixpkgs> {} }:

let
  my-xmonad = import ./default.nix {};
in
  pkgs.lib.overrideDerivation my-xmonad.env (old: {
    buildInputs = with pkgs; old.buildInputs ++ [ cabal-install ghcid hlint ];
  })

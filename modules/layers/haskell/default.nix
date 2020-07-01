{ config, lib, pkgs, ... }:

let
  cfg = config.layers.haskell;

in
  { options = {
      layers.haskell = {
        enable = lib.mkEnableOption "haskell layer";
      };
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
        cabal-install
        cabal-plan
        cabal2nix
        ghcid
        ghcide
        hlint
        ormolu
      ];

      home.file."haskell" = {
        source = ./files;
        target = ".";
        recursive = true;
      };
    };
  }

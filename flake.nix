{
  description = "dotfiles";

  inputs = {
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-overlay.url = "github:evanrelf/haskell-overlay";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kakoune = {
      url = "github:mawww/kakoune";
      flake = false;
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    roc = {
      url = "github:roc-lang/roc";
      inputs.flake-compat.follows = "flake-compat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    roc2nix = {
      url = "github:JRMurr/roc2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.roc.follows = "roc";
    };
    systems.url = "github:nix-systems/default";
    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
      inputs.flake-compat.follows = "flake-compat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            config = { };
            overlays = [
              (_: _: { inherit inputs inputs'; })
              (_: _: { crane = inputs.crane.mkLib pkgs; })
              inputs.fenix.overlays.default
              inputs.haskell-overlay.overlays.default
              inputs.zig-overlay.overlays.default
              (import ./overlays/evan.nix)
              (import ./overlays/haskell.nix)
              (import ./overlays/roc.nix)
              (import ./overlays/bash.nix)
              (import ./overlays/packages.nix)
              (import ./overlays/kakoune-plugins.nix)
              (import ./overlays/home-configurations.nix)
              (import ./overlays/nixos-images.nix)
            ];
          };

        legacyPackages = pkgs;

        devShells = {
          default = config.devShells.everything;

          everything =
            pkgs.mkShell {
              name = "everything";
              packages = [ pkgs.commonPackages ];
            };
        };

        formatter = pkgs.nixpkgs-fmt;
      };
    };
}

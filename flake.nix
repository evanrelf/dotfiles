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
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            config = { };
            overlays = [
              (_: _: { inherit inputs; })
              (_: _: { crane = inputs.crane.lib.${system}; })
              inputs.fenix.overlays.default
              inputs.haskell-overlay.overlays.default
              (import ./overlays/evan.nix)
              (import ./overlays/haskell.nix)
              (import ./overlays/rust.nix)
              (import ./overlays/packages.nix)
              (import ./overlays/kakoune-plugins.nix)
              (import ./overlays/home-configurations.nix)
              (import ./overlays/fonts.nix)
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

  nixConfig = {
    extra-substituters = "https://nix-community.cachix.org https://evanrelf.cachix.org";
    extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= evanrelf.cachix.org-1:n9mrgldEeLIlie/UEGulvshb2Yf5bxz1ZYUIvV5kdO4=";
  };
}

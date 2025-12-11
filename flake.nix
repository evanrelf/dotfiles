{
  description = "dotfiles";

  inputs = {
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    ghciwatch-compat = {
      url = "github:evanrelf/ghciwatch-compat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";

    # Rust programs
    empath = { url = "github:evanrelf/empath"; flake = false; };
    hsl = { url = "github:evanrelf/hsl"; flake = false; };
    pancase = { url = "github:evanrelf/pancase"; flake = false; };

    # Kakoune plugins
    better-haskell-kak = { url = "github:evanrelf/better-haskell.kak"; flake = false; };
    byline-kak = { url = "github:evanrelf/byline.kak"; flake = false; };
    locus-kak = { url = "github:evanrelf/locus.kak"; flake = false; };
    open-github-kak = { url = "github:evanrelf/open-github.kak"; flake = false; };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            config = { allowUnfree = true; };
            overlays = [
              (_: _: { inherit inputs inputs'; })
              inputs.nix-darwin.overlays.default
              inputs.ghciwatch-compat.overlays.default
              (import ./overlays/packages.nix)
              (import ./overlays/kakoune-plugins.nix)
              (import ./overlays/fish-plugins.nix)
              (import ./overlays/darwin-configurations.nix)
              (import ./overlays/home-configurations.nix)
              (import ./overlays/nixos-images.nix)
            ];
          };

        legacyPackages = pkgs;
      };
    };
}

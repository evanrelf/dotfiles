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
    # kakoune = {
    #   url = "github:mawww/kakoune";
    #   flake = false;
    # };
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";
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
              (import ./overlays/evan.nix)
              inputs.ghciwatch-compat.overlays.default
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

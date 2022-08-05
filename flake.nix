{
  description = "dotfiles";

  inputs = {
    comma = {
      url = "github:nix-community/comma";
      inputs.flake-compat.follows = "flake-compat";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlays = [
          (_: _: { inherit inputs; })
          (_: _: { inherit (inputs.comma.packages.${system}) comma; })
          (_: _: { crane = inputs.crane.lib.${system}; })
          (import ./overlays/rust.nix)
          (import ./overlays/packages.nix)
          (import ./overlays/kakoune-plugins.nix)
          (import ./overlays/home-configurations.nix)
          (import ./overlays/fonts.nix)
        ];

        pkgs = import nixpkgs { inherit system config overlays; };
      in
      {
        packages = pkgs;
      }
    );
}

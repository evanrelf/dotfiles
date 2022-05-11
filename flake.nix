{
  description = "dotfiles";

  inputs = {
    comma = {
      url = "github:nix-community/comma";
      inputs.flake-compat.follows = "flake-compat";
      inputs.naersk.follows = "naersk";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
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
    naersk = {
      url = "github:nix-community/naersk";
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
          inputs.naersk.overlay
          (import ./overlays/pkgs.nix)
          (import ./overlays/home-configurations.nix)
        ];

        pkgs = import nixpkgs { inherit system config overlays; };
      in
      {
        packages = pkgs;
      }
    );
}

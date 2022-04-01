{
  description = "dotfiles";

  inputs = {
    comma = {
      url = "github:evanrelf/comma";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    helix = {
      url = "github:helix-editor/helix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
          (_: _: { inherit (inputs.helix.packages.${system}) helix; })
          (import ./overlays/home-configurations.nix)
        ];

        pkgs = import nixpkgs { inherit system config overlays; };
      in
      {
        packages = pkgs;
      }
    );
}

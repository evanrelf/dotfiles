{
  description = "dotfiles";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlays = [ ];

        pkgs = import nixpkgs { inherit system config overlays; };
      in
      {
        packages = pkgs;
      }
    );
}

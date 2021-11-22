{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages = import ./default.nix { inherit system inputs; };
    });
}

{
  description = "evanrelf's dotfiles";

  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    comma = {
      url = "github:evanrelf/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    fenix = {
      url = "github:nix-community/fenix";
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
    impermanence.url = "github:nix-community/impermanence";
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs-slow.url = "github:NixOS/nixpkgs";
    tpm = {
      url = "github:tmux-plugins/tpm";
      flake = false;
    };
    vim-plug = {
      url = "github:junegunn/vim-plug";
      flake = false;
    };
    zig-overlay = {
      url = "github:arqv/zig-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, flake-utils, nixpkgs, nixpkgs-slow, ... }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      defaultPackage = packages.home-rebuild;
      packages =
        let
          config = { };
          overlays = [
            # Make flake inputs available in overlays
            (_: _: { inherit inputs; })
            # Make packages for other systems available for cross compilation
            (_: _: flake-utils.lib.eachDefaultSystem (system: {
              cross = import nixpkgs { inherit system config overlays; };
            }))
            # Provide slowly updated package set to avoid expensive rebuilds of
            # customized derivations (e.g. `emacsGcc`)
            (_: _: {
              slow = import nixpkgs-slow { inherit system config overlays; };
            })
            inputs.emacs-overlay.overlay
            inputs.fenix.overlay
            (import ./overlays/kakoune-plugins.nix)
            (import ./overlays/top-level.nix)
            (import ./overlays/nixos-isos.nix)
            (import ./overlays/home-configurations.nix)
            (import ./overlays/nixos-configurations.nix)
          ];
        in
        import nixpkgs { inherit system config overlays; };
    });
}

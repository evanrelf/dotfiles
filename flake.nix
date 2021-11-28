{
  description = "evanrelf's dotfiles";

  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    comma = {
      url = "github:DavHau/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
    tpm = {
      url = "github:tmux-plugins/tpm";
      flake = false;
    };
    vim-plug = {
      url = "github:junegunn/vim-plug";
      flake = false;
    };
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      defaultPackage = packages.dotfiles;
      packages =
        let
          config = { };
          overlays = [
            # Make flake inputs available in overlays
            (_: _: { inherit inputs; })
            # Make packages for other systems available for cross compilation
            (_: _: flake-utils.lib.eachDefaultSystem (system:
              { cross = import nixpkgs { inherit system config overlays; }; }
            ))
            inputs.emacs-overlay.overlay
            (import ./overlays/kakoune-plugins.nix)
            (import ./overlays/top-level.nix)
            (import ./overlays/home-configurations.nix)
          ];
        in
        import nixpkgs { inherit system config overlays; };
    });
}

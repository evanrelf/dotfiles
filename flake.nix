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
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";

    # Go programs

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

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      flake = {
        overlays.default =
          inputs.nixpkgs.lib.composeManyExtensions [
            (_: _: { inherit inputs; })
            inputs.ghciwatch-compat.overlays.default
            inputs.llm-agents.overlays.default
            inputs.naersk.overlays.default
            inputs.nix-darwin.overlays.default
            (import ./overlays/packages.nix)
            (import ./overlays/kakoune-plugins.nix)
            (import ./overlays/fish-plugins.nix)
            (import ./overlays/nixos-configurations.nix)
            (import ./overlays/darwin-configurations.nix)
            (import ./overlays/home-configurations.nix)
            (import ./overlays/container-images.nix)
          ];
      };

      perSystem = { inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            inherit system;
            config = { allowUnfree = true; };
            overlays = [ inputs.self.overlays.default ];
          };

        legacyPackages = pkgs;
      };
    };
}

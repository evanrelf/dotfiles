{
  description = "dotfiles";

  inputs = {
    crane.url = "github:ipetkov/crane";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    import-tree.url = "github:vic/import-tree/a037ed2"; # v0.1.0
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";

    # Go programs

    # Rust programs
    empath = { url = "github:evanrelf/empath"; flake = false; };
    hsl = { url = "github:evanrelf/hsl"; flake = false; };
    indigo = { url = "github:evanrelf/indigo"; };
    pancase = { url = "github:evanrelf/pancase"; flake = false; };

    # Kakoune plugins
    better-haskell-kak = { url = "github:evanrelf/better-haskell.kak"; flake = false; };
    byline-kak = { url = "github:evanrelf/byline.kak"; flake = false; };
    locus-kak = { url = "github:evanrelf/locus.kak"; flake = false; };
    open-github-kak = { url = "github:evanrelf/open-github.kak"; flake = false; };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake
      { inherit inputs; }
      (inputs.import-tree ./modules/flake);

  nixConfig = {
    extra-substituters = [
      "https://cache.numtide.com"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}

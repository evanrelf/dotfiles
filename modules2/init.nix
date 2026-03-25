{ config, inputs, lib, ... }:

{
  systems = import inputs.systems;

  flake.overlays.default = lib.composeManyExtensions [
    (_: _: { inherit inputs; })
    inputs.claude-mergetool.overlays.default
    inputs.ghciwatch-compat.overlays.default
    inputs.llm-agents.overlays.default
    inputs.naersk.overlays.default
    inputs.neovim.overlays.default
    inputs.nix-darwin.overlays.default
    (import ../overlays/packages.nix)
    (import ../overlays/kakoune-plugins.nix)
    (import ../overlays/fish-plugins.nix)
    (import ../overlays/nixos-configurations.nix)
    (import ../overlays/darwin-configurations.nix)
    (import ../overlays/home-configurations.nix)
    (import ../overlays/container-images.nix)
  ];

  perSystem = { pkgs, system, ... }: {
    _module.args.pkgs =
      import inputs.nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [ config.flake.overlays.default ];
      };

    legacyPackages = pkgs;
  };
}

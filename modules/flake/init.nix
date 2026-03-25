{ config, inputs, lib, ... }:

let
  darwinModulesOption = { lib, moduleLocation, ... }: {
    options.flake.darwinModules = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.deferredModule;
      default = { };
      apply = lib.mapAttrs (
        k: v: {
          _class = "darwin";
          _file = "${toString moduleLocation}#darwinModules.${k}";
          imports = [ v ];
        }
      );
      description = "Darwin modules";
    };
  };

in
{
  imports = [
    inputs.home-manager.flakeModules.default
    inputs.nix-darwin.flakeModules.default
    darwinModulesOption
  ];

  systems = import inputs.systems;

  flake.overlays.default = lib.composeManyExtensions [
    (_: _: { inherit inputs; })
    inputs.claude-mergetool.overlays.default
    inputs.ghciwatch-compat.overlays.default
    inputs.llm-agents.overlays.default
    inputs.naersk.overlays.default
    inputs.neovim.overlays.default
    inputs.nix-darwin.overlays.default
    (import ../../overlays/packages.nix)
    (import ../../overlays/kakoune-plugins.nix)
    (import ../../overlays/fish-plugins.nix)
    (import ../../overlays/nixos-configurations.nix)
    (import ../../overlays/home-configurations.nix)
    (import ../../overlays/container-images.nix)
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

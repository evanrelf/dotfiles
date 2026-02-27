{ inputs, withSystem, ... }:

let
  mkDarwinConfiguration = modules:
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.nix-darwin.lib.darwinSystem {
        inherit pkgs modules;
        specialArgs = { inherit inputs; };
      }
    );

in
{
  flake.darwinConfigurations = {
    pearl =
      mkDarwinConfiguration [ ../darwin/machines/pearl.nix ];

    ultraviolet =
      mkDarwinConfiguration [ ../darwin/machines/ultraviolet.nix ];
  };
}

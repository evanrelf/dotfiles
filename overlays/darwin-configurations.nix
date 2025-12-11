final: prev:

let
  mkDarwinConfiguration = modules:
    final.inputs.nix-darwin.lib.darwinSystem {
      pkgs = final;
      inherit modules;
      specialArgs = { inherit (prev) inputs; };
    };

in
{
  darwinConfigurations = {
    pearl =
      mkDarwinConfiguration [ ../modules/darwin/machines/pearl.nix ];

    ultraviolet =
      mkDarwinConfiguration [ ../modules/darwin/machines/ultraviolet.nix ];
  };
}

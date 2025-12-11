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
    ultraviolet =
      mkDarwinConfiguration [ ../modules/darwin/machines/ultraviolet.nix ];
  };
}

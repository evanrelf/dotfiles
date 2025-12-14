final: prev:

let
  mkHomeConfiguration = modules:
    final.lib.fix (self:
      (final.inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = final;
        inherit modules;
        extraSpecialArgs = { inherit (prev) inputs; };
      }) // { default = self.activationPackage; }
    );

in
{
  commonPackages =
    (import ../modules/home/common.nix {
      config = { };
      inputs = final.inputs;
      lib = final.lib;
      pkgs = final;
    }).home.packages;

  homeConfigurations = {
    lima =
      mkHomeConfiguration [ ../modules/home/machines/lima.nix ];

    pearl =
      mkHomeConfiguration [ ../modules/home/machines/pearl.nix ];

    ultraviolet =
      mkHomeConfiguration [ ../modules/home/machines/ultraviolet.nix ];
  };
}

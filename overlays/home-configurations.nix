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
    (import ../home/common.nix {
      config = { };
      inputs = final.inputs;
      lib = final.lib;
      pkgs = final;
    }).home.packages;

  homeConfigurations = {
    lima-default =
      mkHomeConfiguration [ ../home/machines/lima-default.nix ];

    pearl =
      mkHomeConfiguration [ ../home/machines/pearl.nix ];

    porcelain =
      mkHomeConfiguration [ ../home/machines/porcelain.nix ];

    ultraviolet =
      mkHomeConfiguration [ ../home/machines/ultraviolet.nix ];

    ultraviolet-vm =
      mkHomeConfiguration [ ../home/machines/ultraviolet-vm.nix ];
  };
}

pkgsFinal: pkgsPrev:

let
  mkHomeConfiguration = modules:
    pkgsFinal.lib.fix (self:
      (pkgsFinal.inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = pkgsFinal;
        inherit modules;
        extraSpecialArgs = { inherit (pkgsPrev) inputs; };
      }) // { default = self.activationPackage; }
    );

in
{
  homeConfigurations = {
    porcelain =
      mkHomeConfiguration [ ../home/machines/porcelain.nix ];

    ultraviolet =
      mkHomeConfiguration [ ../home/machines/ultraviolet.nix ];

    ultraviolet-vm =
      mkHomeConfiguration [ ../home/machines/ultraviolet-vm.nix ];
  };
}

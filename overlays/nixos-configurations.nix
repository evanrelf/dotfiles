final: prev:

let
  mkNixOSConfiguration = modules:
    final.inputs.nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      pkgs = final;
      inherit modules;
      specialArgs = { inherit (prev) inputs; };
    };

in
{
  nixosConfigurations = {
    vm =
      mkNixOSConfiguration [ ../modules/nixos/machines/vm.nix ];

    vm-installer =
      mkNixOSConfiguration [ ../modules/nixos/machines/vm-installer.nix ];
  };

  nixosImages = {
    vm-installer =
      final.inputs.nixos-generators.nixosGenerate {
        pkgs = final;
        modules = [ ../modules/nixos/machines/vm-installer.nix ];
        format = "install-iso";
        specialArgs = { inherit (prev) inputs; };
      };
  };
}

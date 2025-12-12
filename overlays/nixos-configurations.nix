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
    lima =
      mkNixOSConfiguration [ ../modules/nixos/machines/lima.nix ];

    vm =
      mkNixOSConfiguration [ ../modules/nixos/machines/vm.nix ];

    # nix build --system aarch64-linux --max-jobs 0 .#nixosConfigurations.vm-installer.config.system.build.isoImage
    vm-installer =
      mkNixOSConfiguration [ ../modules/nixos/machines/vm-installer.nix ];
  };

  nixosImages = {
    lima =
      final.inputs.nixos-generators.nixosGenerate {
        pkgs = final;
        modules = [ ../modules/nixos/machines/lima.nix ];
        format = "qcow-efi";
        specialArgs = { inherit (prev) inputs; };
      };
  };
}

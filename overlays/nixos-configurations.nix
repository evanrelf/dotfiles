final: prev:

{
  nixosConfigurations = {
    vm =
      final.inputs.nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ ../modules/nixos/machines/vm.nix ];
        pkgs = final;
        specialArgs = { inherit (prev) inputs; };
      };

    vm-installer =
      final.inputs.nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ ../modules/nixos/machines/vm-installer.nix ];
        pkgs = final;
        specialArgs = { inherit (prev) inputs; };
      };
  };

  nixosImages = {
    vm-installer =
      final.nixosConfigurations.vm-installer.config.system.build.images.iso-installer;
  };
}

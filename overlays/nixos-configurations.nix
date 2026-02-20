final: prev:

{
  nixosConfigurations = {
    iris =
      final.inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ../modules/nixos/machines/iris/configuration.nix ];
        pkgs = final;
        specialArgs = { inherit (prev) inputs; };
      };

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
      final.inputs.nixos-generators.nixosGenerate {
        pkgs = final;
        modules = [ ../modules/nixos/machines/vm-installer.nix ];
        format = "install-iso";
        specialArgs = { inherit (prev) inputs; };
      };
  };
}

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

    # nix build --system aarch64-linux --max-jobs 0 .#nixosConfigurations.vm-installer.config.system.build.isoImage
    vm-installer =
      mkNixOSConfiguration [ ../modules/nixos/machines/vm-installer.nix ];
  };
}

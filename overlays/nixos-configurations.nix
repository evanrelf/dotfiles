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
  };
}

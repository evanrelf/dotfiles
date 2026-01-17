final: prev:

let
  mkNixOSConfiguration = system: modules:
    final.inputs.nixpkgs.lib.nixosSystem {
      inherit system modules;
      pkgs = final;
      specialArgs = { inherit (prev) inputs; };
    };

in
{
  nixosConfigurations = {
    iris =
      mkNixOSConfiguration
        "x86_64-linux"
        [ ../modules/nixos/machines/iris/configuration.nix ];

    vm =
      mkNixOSConfiguration
        "aarch64-linux"
        [ ../modules/nixos/machines/vm.nix ];

    vm-installer =
      mkNixOSConfiguration
        "aarch64-linux"
        [ ../modules/nixos/machines/vm-installer.nix ];
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

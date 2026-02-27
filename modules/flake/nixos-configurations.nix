{ inputs, withSystem, ... }:

let
  mkNixosConfiguration = system: modules:
    withSystem system ({ pkgs, ... }:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs modules;
        specialArgs = { inherit inputs; };
      }
    );

in
{
  flake.nixosConfigurations = {
    iris =
      mkNixosConfiguration "x86_64-linux" [ ../nixos/machines/iris/configuration.nix ];

    vm =
      mkNixosConfiguration "aarch64-linux" [ ../nixos/machines/vm.nix ];

    vm-installer =
      mkNixosConfiguration "aarch64-linux" [ ../nixos/machines/vm-installer.nix ];
  };

  flake.nixosImages = {
    vm-installer =
      inputs.self.nixosConfigurations.vm-installer.config.system.build.images.iso-installer;
  };
}

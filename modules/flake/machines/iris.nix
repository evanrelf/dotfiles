{ config, inputs, withSystem, ... }:

{
  flake.nixosConfigurations.iris =
    withSystem "x86_64-linux" ({ pkgs, ... }:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        system = "x86_64-linux";
        modules = [ config.flake.nixosModules.iris ];
        specialArgs = { inherit inputs; };
      }
    );

  flake.nixosModules.iris =
    import ../../nixos/machines/iris/configuration.nix;

  flake.homeConfigurations.iris =
    withSystem "x86_64-linux" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.iris ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.iris = { config, ... }: {
    imports = [
      ../../home/common.nix
    ];

    home.stateVersion = "25.11";
  };
}

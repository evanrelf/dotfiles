{ config, inputs, withSystem, ... }:

{
  flake.darwinConfigurations.ultraviolet =
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.nix-darwin.lib.darwinSystem {
        inherit pkgs;
        modules = [ config.flake.darwinModules.ultraviolet ];
        specialArgs = { inherit inputs; };
      }
    );

  flake.darwinModules.ultraviolet = { config, ... }: {
    imports = [
      ../../darwin/common.nix
    ];

    system.stateVersion = 6;
  };

  flake.homeConfigurations.ultraviolet =
    withSystem "aarch64-darwin" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.ultraviolet ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.ultraviolet = { pkgs, ... }: {
    imports = [
      ../../home/common.nix
    ];

    home.packages = with pkgs; [
      claude-code
    ];

    home.stateVersion = "22.11";
  };
}

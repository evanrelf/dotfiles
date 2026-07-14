{ config, inputs, withSystem, ... }:

{
  flake.homeConfigurations.evanrelf-desktop =
    withSystem "x86_64-linux" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.evanrelf-desktop ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.evanrelf-desktop = { pkgs, ... }: {
    imports = [
      ../../home/common.nix
    ];

    home.packages = with pkgs; [
      ghostty
      wl-clipboard
    ];

    home.stateVersion = "26.05";
  };
}

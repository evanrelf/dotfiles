{ config, inputs, lib, withSystem, ... }:

{
  flake.homeConfigurations.lima =
    withSystem "aarch64-linux" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ config.flake.homeModules.lima ];
        extraSpecialArgs = { inherit inputs; };
      }
    );

  flake.homeModules.lima = { config, ... }: {
    imports = [
      ../../../home/common.nix
    ];

    home.homeDirectory = lib.mkForce "/home/${config.home.username}.linux";

    home.file."Code/${config.home.username}/dotfiles".source =
      config.lib.file.mkOutOfStoreSymlink "/Users/${config.home.username}/Code/${config.home.username}/dotfiles";

    home.stateVersion = "25.11";
  };
}

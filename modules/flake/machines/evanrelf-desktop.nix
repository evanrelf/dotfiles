{ config, inputs, withSystem, ... }:

let
  system = "x86_64-linux";

in
{
  flake.homeConfigurations.evanrelf-desktop =
    withSystem system ({ pkgs, ... }:
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
      file
      # TODO: Go back to `pkgs.ghostty` once my Nixpkgs has v1.4.0
      inputs.nixpkgs-master.legacyPackages.${system}.ghostty
      gnome-tweaks
      vscode
      wl-clipboard
      zed-editor
    ];

    home.stateVersion = "26.05";
  };
}

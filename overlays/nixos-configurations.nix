pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs;
  inherit (inputs) impermanence nixpkgs;

  mkConfiguration = hostname: { ... }:
    nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        impermanence.nixosModules.impermanence
        (../nixos/machines + "/${hostname}/hardware-configuration.nix")
        (../nixos/machines + "/${hostname}/configuration.nix")
      ];
      specialArgs = { inherit inputs; };
    };

in
{
  nixosConfigurations = builtins.mapAttrs mkConfiguration {
    "iris" = { };
    "sienna" = { };
  };
}

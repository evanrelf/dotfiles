pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs;
  inherit (inputs) impermanence nixpkgs;

  mkConfiguration = hostname: { ... }:
    nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        impermanence.nixosModules.impermanence
        ../nixos/git-rev.nix
        (../nixos/machines + "/${hostname}")
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

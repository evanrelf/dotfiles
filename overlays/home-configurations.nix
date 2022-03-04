pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs config overlays;
  inherit (inputs) home-manager nixpkgs;

in
{
  homeConfigurations = {
    ultraviolet = home-manager.lib.homeManagerConfiguration rec {
      system = "aarch64-darwin";
      username = "evanrelf";
      homeDirectory = "/Users/${username}";
      pkgs = import nixpkgs { inherit system config overlays; };
      configuration.imports = [ ../home/machines/ultraviolet.nix ];
    };
  };
}

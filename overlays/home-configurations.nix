pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs config overlays;
  inherit (inputs) home-manager nixpkgs;

in
{
  homeConfigurations = {
    hydra-dev = home-manager.lib.homeManagerConfiguration rec {
      pkgs = pkgsFinal;
      modules = [ ../home/machines/hydra-dev.nix ];
    };

    indigo = home-manager.lib.homeManagerConfiguration rec {
      pkgs = pkgsFinal;
      modules = [ ../home/machines/indigo.nix ];
    };

    porcelain = home-manager.lib.homeManagerConfiguration rec {
      pkgs = pkgsFinal;
      modules = [ ../home/machines/porcelain.nix ];
    };

    ultraviolet = home-manager.lib.homeManagerConfiguration rec {
      pkgs = pkgsFinal;
      modules = [ ../home/machines/ultraviolet.nix ];
    };
  };
}

pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs config overlays;
  inherit (inputs) home-manager nixpkgs;

in
{
  homeConfigurations = {
    hydra-dev = home-manager.lib.homeManagerConfiguration rec {
      system = "x86_64-linux";
      username = "evan";
      homeDirectory = "/home/${username}";
      pkgs = import nixpkgs { inherit system config overlays; };
      extraSpecialArgs = { inherit inputs; };
      configuration.imports = [ ../home/machines/hydra-dev.nix ];
    };

    indigo = home-manager.lib.homeManagerConfiguration rec {
      system = "x86_64-darwin";
      username = "evan";
      homeDirectory = "/Users/${username}";
      pkgs = import nixpkgs { inherit system config overlays; };
      extraSpecialArgs = { inherit inputs; };
      configuration.imports = [ ../home/machines/indigo.nix ];
    };

    ultraviolet = home-manager.lib.homeManagerConfiguration rec {
      system = "aarch64-darwin";
      username = "evanrelf";
      homeDirectory = "/Users/${username}";
      pkgs = import nixpkgs { inherit system config overlays; };
      extraSpecialArgs = { inherit inputs; };
      configuration.imports = [ ../home/machines/ultraviolet.nix ];
    };
  };
}

pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) inputs config overlays;
  inherit (inputs) home-manager nixpkgs;

  mkConfiguration = hostname: { system, username }:
    home-manager.lib.homeManagerConfiguration rec {
      inherit system username;
      homeDirectory =
        if builtins.match ".*-darwin" system != null then
          "/Users/${username}"
        else
          "/home/${username}";
      pkgs = import nixpkgs { inherit system config overlays; };
      extraSpecialArgs = { inherit inputs; };
      extraModules = import ../home;
      configuration.imports = [ (../home/machines + "/${hostname}.nix") ];
    };

in
{
  homeConfigurations = builtins.mapAttrs mkConfiguration {
    # Personal
    "ultraviolet" = { system = "aarch64-darwin"; username = "evanrelf"; };
    "iris" = { system = "x86_64-linux"; username = "evan"; };
    "auburn" = { system = "x86_64-darwin"; username = "evanrelf"; };
    "sienna" = { system = "x86_64-linux"; username = "evanrelf"; };
    # Work
    "indigo" = { system = "x86_64-darwin"; username = "evan"; };
    "hydra-dev" = { system = "x86_64-linux"; username = "evan"; };
  };
}

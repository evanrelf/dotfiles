{
  description = "evanrelf's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      eachSystem = f:
        builtins.listToAttrs
          (builtins.map
            (system: { name = system; value = f system; })
            [
              "aarch64-darwin"
              "x86_64-darwin"
              "x86_64-linux"
            ]);
    in
    {
      # e.g. `nix run . -- switch`
      defaultApp = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };

        in
        {
          type = "app";
          # TODO:
          #
          # Silence this error message when `build`ing or `switch`ing:
          #
          #```
          # No configuration file found. Please create one at /Users/evanrelf/.config/nixpkgs/home.nix
          #```
          #
          # One of these should work:
          #
          # ```
          # export HOME_MANAGER_CONFIG="/dev/null"
          # export HOME_MANAGER_CONFIG="${pkgs.writeText "home.nix" "{ }"}"
          # ```
          #
          # ...but it produces this error message:
          #
          # ```
          # error: file 'home-manager/home-manager/home-manager.nix' was not found in the Nix search path (add it using $NIX_PATH or -I)
          # ```
          program = builtins.toString (pkgs.writeShellScript "home-manager" ''
            export PATH="${home-manager.defaultPackage."${system}"}/bin:$PATH"
            hostname=$(hostname -s)
            { set -x; home-manager --flake .#$hostname $@; }
          '');
        }
      );

      homeConfigurations = rec {
        "ultraviolet" = home-manager.lib.homeManagerConfiguration rec {
          system = "aarch64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/ultraviolet.nix ];
        };

        "auburn" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/auburn.nix ];
        };

        "sienna" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evanrelf";
          homeDirectory = "/home/${username}";
          configuration.imports = [ ./modules/machines/sienna.nix ];
        };

        "indigo" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evan";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/indigo.nix ];
        };

        "hydra-dev" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evan";
          homeDirectory = "/home/${username}";
          configuration.imports = [ ./modules/machines/hydra-dev.nix ];
        };
      };
    };
}

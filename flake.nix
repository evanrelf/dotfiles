{
  description = "evanrelf's dotfiles";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
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
      defaultPackage = eachSystem (system:
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
        let
          pkgs = import nixpkgs { inherit system; };

        in
        # e.g. `nix run . -- switch` or `eval $(nix-build --no-out-link)/bin/home-manager switch`
        pkgs.writeShellScriptBin "home-manager" ''
          export PATH="${home-manager.defaultPackage."${system}"}/bin:$PATH"
          hostname=$(hostname -s)
          trace() { set -x; $@; { set +x; } 2>/dev/null; }
          if [ "$(nix-instantiate --eval --expr 'builtins ? getFlake')" = "true" ]; then
            trace home-manager --flake .#$hostname $@
          else
            if [ "$#" = "1" ] && [ "$1" = "switch" ]; then
              echo "Falling back to non-flake switch"
              temp=$(mktemp -d)
              trap "rm -rf $temp" EXIT
              trace nix build --file . homeConfigurations.$hostname -o $temp/result
              config=$(readlink "$temp/result")
              trace $config/activate
            elif [ "$#" = "1" ] && [ "$1" = "build" ]; then
              echo "Falling back to non-flake build"
              trace nix build --file . homeConfigurations.$hostname
            else
              echo "Unsupported arguments in fallback mode"
              trace home-manager $@
            fi
          fi
        ''
      );

      homeConfigurations = {
        "ultraviolet" = home-manager.lib.homeManagerConfiguration rec {
          system = "aarch64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/ultraviolet.nix ];
          extraSpecialArgs = { inherit inputs; };
        };

        "auburn" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/auburn.nix ];
          extraSpecialArgs = { inherit inputs; };
        };

        "sienna" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evanrelf";
          homeDirectory = "/home/${username}";
          configuration.imports = [ ./modules/machines/sienna.nix ];
          extraSpecialArgs = { inherit inputs; };
        };

        "indigo" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evan";
          homeDirectory = "/Users/${username}";
          configuration.imports = [ ./modules/machines/indigo.nix ];
          extraSpecialArgs = { inherit inputs; };
        };

        "hydra-dev" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evan";
          homeDirectory = "/home/${username}";
          configuration.imports = [ ./modules/machines/hydra-dev.nix ];
          extraSpecialArgs = { inherit inputs; };
        };
      };
    };
}

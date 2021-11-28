{
  description = "evanrelf's dotfiles";

  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    tpm = {
      url = "github:tmux-plugins/tpm";
      flake = false;
    };
    vim-plug = {
      url = "github:junegunn/vim-plug";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      config = { };

      overlays = [
        (_: _: eachSystem (system:
          import nixpkgs { inherit system config overlays; }
        ))
        (_: { system, ... }: {
          inherit (home-manager.packages."${system}") home-manager;
        })
        inputs.emacs-overlay.overlay
        (import ./overlays/kakoune-plugins.nix)
        (import ./overlays/top-level.nix)
      ];

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
      defaultPackage = eachSystem (system: self.packages."${system}".dotfiles);

      packages = eachSystem (system:
        import nixpkgs { inherit system config overlays; }
      );

      homeConfigurations = {
        "ultraviolet" = home-manager.lib.homeManagerConfiguration rec {
          system = "aarch64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          pkgs = import nixpkgs { inherit system config overlays; };
          extraSpecialArgs = { inherit inputs; };
          extraModules = import ./modules;
          configuration.imports = [ ./modules/machines/ultraviolet.nix ];
        };

        "auburn" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evanrelf";
          homeDirectory = "/Users/${username}";
          pkgs = import nixpkgs { inherit system config overlays; };
          extraSpecialArgs = { inherit inputs; };
          extraModules = import ./modules;
          configuration.imports = [ ./modules/machines/auburn.nix ];
        };

        "sienna" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evanrelf";
          homeDirectory = "/home/${username}";
          pkgs = import nixpkgs { inherit system config overlays; };
          extraSpecialArgs = { inherit inputs; };
          extraModules = import ./modules;
          configuration.imports = [ ./modules/machines/sienna.nix ];
        };

        "indigo" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-darwin";
          username = "evan";
          homeDirectory = "/Users/${username}";
          pkgs = import nixpkgs { inherit system config overlays; };
          extraSpecialArgs = { inherit inputs; };
          extraModules = import ./modules;
          configuration.imports = [ ./modules/machines/indigo.nix ];
        };

        "hydra-dev" = home-manager.lib.homeManagerConfiguration rec {
          system = "x86_64-linux";
          username = "evan";
          homeDirectory = "/home/${username}";
          pkgs = import nixpkgs { inherit system config overlays; };
          extraSpecialArgs = { inherit inputs; };
          extraModules = import ./modules;
          configuration.imports = [ ./modules/machines/hydra-dev.nix ];
        };
      };
    };
}

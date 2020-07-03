let
  pkgs = import ./nixpkgs.nix { overlays = import ./overlays; };

in
  pkgs.mkShell {
    buildInputs = with pkgs; [ home-manager ];

    shellHook = ''
      export NIX_PATH="nixpkgs=${pkgs.path}:nixpkgs-overlays=./overlays/default.nix:home-manager=${pkgs.home-manager}"
      export HOME_MANAGER_CONFIG="./home.nix"
    '';
  }

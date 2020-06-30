let
  pkgs = import ./nixpkgs.nix { overlays = [ (import ./overlay.nix) ]; };

in
  pkgs.mkShell {
    buildInputs = with pkgs; [ home-manager ];

    shellHook = ''
      export NIX_PATH="nixpkgs=${pkgs.path}:home-manager=${pkgs.home-manager}"
      export HOME_MANAGER_CONFIG="./home.nix"
    '';
  }

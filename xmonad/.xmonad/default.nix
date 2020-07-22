let
  pkgs = import <nixpkgs> { config = {}; };

  src =
    builtins.path {
      name = "my-xmonad";
      path = ./.;
      filter =
        pkgs.nix-gitignore.gitignoreFilterPure (_: _: true)
          [ "/dist-newstyle/"
            "/result"
            "/result-*"
            "*.nix"
          ]
          ./.;
    };

in
  pkgs.haskell.lib.justStaticExecutables
    (pkgs.haskellPackages.callCabal2nix "my-xmonad" src {})

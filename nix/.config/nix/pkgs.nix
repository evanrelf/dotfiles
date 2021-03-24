args:

import ./nixpkgs.nix ({
  overlays = [
    (import ./overlays/top-level.nix)
    (import ./overlays/haskell-packages.nix)
    (import ./overlays/kakoune-plugins.nix)
  ];
} // args)

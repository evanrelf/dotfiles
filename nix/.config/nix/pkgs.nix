args:

import ./nixpkgs.nix ({
  overlays = [
    (import ./overlays/top-level.nix)
    (import ./overlays/haskell.nix)
  ];
} // args)

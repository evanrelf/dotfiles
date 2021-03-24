args:

import ./nixpkgs.nix ({
  overlays = [
    (import ./overlays/top-level.nix)
    (import ./overlays/haskell.nix)
    # (import ./overlays/kakoune-plugins.nix)
  ];
} // args)

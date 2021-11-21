args:

import ./nixpkgs.nix ({
  overlays = [
    (import ./overlays/emacs.nix)
    (import ./overlays/kakoune-plugins.nix)
    (import ./overlays/top-level.nix)
  ];
} // args)

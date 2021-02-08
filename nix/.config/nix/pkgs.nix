args:

import ./nixpkgs.nix ({ overlays = [ (import ./overlay.nix) ]; } // args)

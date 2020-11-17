args:

import ./nixpkgs.nix ({ overlays = import ./overlays; } // args)

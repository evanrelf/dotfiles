let
  rev = "a72e2a008e2551b2e4bcbae0c20e7e6e6f91dfc6";
  sha256 = "1xpgs2hzhkmwdbrd264ikgf6j6jj6s779bjqj5x09lw1gvv93i1r";

in
import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  inherit sha256;
})

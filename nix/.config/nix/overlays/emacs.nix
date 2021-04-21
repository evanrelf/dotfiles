let
  rev = "cf8b6a5608feaf3b0cf864c057b4cb6ab760a269";
  sha256 = "1nrjmb59qy9m5smhz3s1amgz2clj1sxv452xxjcsb6nnghad1zag";

in
import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  inherit sha256;
})

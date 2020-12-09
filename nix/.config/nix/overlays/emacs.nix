let
  rev = "43c916bc555d9531142e1b5e912b4c7639dde916";

in
  import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    sha256 = "0yqqkmy006hjm4ji7q0q99d1z413pxk9mb74915rmhnl8h43ak1l";
  })

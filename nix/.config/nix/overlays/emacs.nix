let
  rev = "5df3462dda05d8e44669cf374776274e1bc47d0a";
  sha256 = "0ggmkg4shf9948wpwb0s40bjvwijvhv2wykrkayclvp419kbrfxq";

in
import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  inherit sha256;
})

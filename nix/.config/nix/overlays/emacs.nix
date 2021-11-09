let
  rev = "3819c4478b64c73e024503f845d50fbf2cf73edc";
  sha256 = "0dd4p162ka72pbpg46qlbh6khyqf3dqkg9gidgihfh5a63riygs3";

in
import (builtins.fetchTarball {
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
  inherit sha256;
})

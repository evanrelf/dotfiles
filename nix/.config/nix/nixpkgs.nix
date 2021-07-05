args:

let
  # master on 2021-07-05
  rev = "490a823e78febc182a70c902c028af4a78a37717";
  sha256 = "1q28snbqq668r9h13hh7csnlrh61y23fyfgmb44ww94i54mk2ifg";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

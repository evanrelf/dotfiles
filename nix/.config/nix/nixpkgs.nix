args:

let
  # master on 2021-05-27
  rev = "339a49503ae85b6cfd110b248010d24252d7aa45";
  sha256 = "0zkljhm1v7iwwa06i62s2s8rbdcbc28nf3ps838c0jv3v32g4ws8";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

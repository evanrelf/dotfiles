args:

let
  # master on 2021-08-12
  rev = "6e4c36b3f7ec1400bd92ef42567b687f20f3c3c4";
  sha256 = "0jq8715g4f8ccmk18df0m0mmq2fdnivaxhx4x61cmkyjps31f868";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

args:

let
  # master on 2021-03-23
  rev = "a57a59febea4287e122001d22be9bbca7932b6ba";
  sha256 = "07nimnssd28cabb4avpkq4qfj9acbzvk1y6fq05501lj5zk5v6m5";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

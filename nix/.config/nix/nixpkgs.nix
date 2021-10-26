args:

let
  # master on 2021-10-26
  rev = "2190ff6ea6ad9d4eaf7a19cd67d9d4ccbaf1093b";
  sha256 = "1ak9q0kafjfddrj8m3q0h637iai0pgzp0gh5w5jhmr9ckh5gqq4h";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

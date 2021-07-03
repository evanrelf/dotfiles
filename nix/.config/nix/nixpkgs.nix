args:

let
  # master on 2021-07-02
  rev = "69063eaf6253160ea6e0d8142147b358c6e28ea2";
  sha256 = "0h3b61z0x2m2wg4pxlygp50f3gllar8dn7y8mxsxkh78blrx2xd6";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

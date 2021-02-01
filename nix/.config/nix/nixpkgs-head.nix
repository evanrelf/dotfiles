args:

let
  # master on 2021-01-31
  rev = "d1c040f8c6139ff47f2508a9f762640078cec69c";
  sha256 = "0qr7dazd2qyyiilimfahkagd9jhsffk547r09z73wm1j35dhvny7";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)

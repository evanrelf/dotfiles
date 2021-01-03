args:

let
  # master on 2021-01-03
  rev = "1d10a2af91d31e0da00b3259cc976c7c0033ba85";
  sha256 = "05iday54fvsrpri8sdmib59w1kq2afwd2zxwgdrhwiaj77s676fc";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)

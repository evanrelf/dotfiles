args:

let
  # master on 2021-03-06
  rev = "61b42c2d0f2b3c92b97190b108074073e39b8e35";
  sha256 = "0lf24crx4z5rc2bqvbf4qhim5kjk3gv62yayznm5sg22wq9qc27b";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)

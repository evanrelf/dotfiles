args:

let
  # master on 2020-07-30
  rev = "b1ac18b7acc54ba6086b51875ea245113821c050";
  nixpkgs =
    builtins.fetchTarball {
      name = "nixpkgs-unstable";
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "0if3i8w5f2p8fm8bfnj0hlx7yhz3b5cqhq5gqzs1i542bzy5d6ab";
    };

in
  import nixpkgs ({ config = {}; } // args)

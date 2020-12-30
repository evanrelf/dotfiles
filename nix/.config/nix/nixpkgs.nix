args:

let
  # master on 2020-12-30
  rev = "9b78f36021b3530773c4a2136c26862d32aef3f6";

  nixpkgs =
    builtins.fetchTarball {
      name = "nixpkgs";
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "1b2apazjm9n4phh6gvq9dw6x6ziklvvqahz7wy5gmbmyaxk8yf1g";
    };

in
  import nixpkgs ({ config = {}; } // args)

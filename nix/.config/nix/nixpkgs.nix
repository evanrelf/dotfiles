args:

let
  # master on 2020-11-17
  rev = "87aa3671e803f32617e899980c72dd7bc308cd1e";
  nixpkgs =
    builtins.fetchTarball {
      name = "nixpkgs-unstable";
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "1xy3ys1z8vh42vmj2i2y0j352p8q0j8hg2zliavdrjz6447q9136";
    };

in
  import nixpkgs ({ config = {}; } // args)

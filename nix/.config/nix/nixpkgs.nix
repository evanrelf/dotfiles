args:

let
  # master on 2020-09-19
  rev = "d61c16cda48a581dfecfce69099773d9737d5b76";
  nixpkgs =
    builtins.fetchTarball {
      name = "nixpkgs-unstable";
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "1y9yc74acxkcw8pxibqafmx8nz622xnd3i9b1yqsbxm4xqwkzl35";
    };

in
  import nixpkgs ({ config = {}; } // args)

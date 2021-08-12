args:

let
  # master on 2021-08-12
  rev = "bbfec00ec916415738ea7b723dd472d7dbafaa18";
  sha256 = "1v9px4kp9ys3sv5zrdxviw5366rv8fvlkfc7kc72xcdlk1sxjmi0";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)

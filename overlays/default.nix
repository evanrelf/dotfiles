let
  contents = builtins.attrNames (builtins.readDir ./.);

  overlays =
    let
      isNixFile = x: builtins.match ".*\\.nix" x != null;

      isDirWithDefault = x: builtins.pathExists (./. + (x + "/default.nix"));

      isNotDefault = x: x != "default.nix";
    in
      builtins.filter
        (x: (isNixFile x || isDirWithDefault x) && isNotDefault x)
        contents;

in
  builtins.map (x: import (./. + ("/" + x))) overlays

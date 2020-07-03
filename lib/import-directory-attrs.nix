path:

let
  # Need to set overlays explicitly here to avoid infinite recursion segfault
  # See this issue for more info: https://github.com/NixOS/nix/issues/3780
  pkgs = import ../nixpkgs.nix { overlays = []; };

  lib = pkgs.lib;

  contents =
    let
      isNixFile = name: builtins.match ".*\\.nix" name != null;

      isDirWithDefault = name: builtins.pathExists (path + ("/" + name + "/default.nix"));

      isNotDefault = name: name != "default.nix";
    in
      lib.filterAttrs
        (name: _: (isNixFile name || isDirWithDefault name) && isNotDefault name)
        (builtins.readDir path);

  adapt = oldName: _:
    { name = lib.removeSuffix ".nix" oldName;
      value = import (path + ("/" + oldName));
    };

in
  lib.mapAttrs' adapt contents

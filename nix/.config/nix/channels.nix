let
  inherit (import ./nixpkgs.nix) stable sources;
in
  stable.linkFarm "channels"
    (stable.lib.mapAttrsToList
      (name: value: { inherit name; path = value; })
      sources)

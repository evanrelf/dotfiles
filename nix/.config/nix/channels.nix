let
  inherit (import ./nixpkgs.nix) stable sources;
in
  # TODO: Add this derivation's build product to my NIX_PATH
  stable.linkFarm "channels"
    (stable.lib.mapAttrsToList
      (name: value: { inherit name; path = value; })
      sources)

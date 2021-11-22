{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      mapAttrNames = f: names:
        builtins.listToAttrs
          (builtins.map
            (name: { inherit name; value = f name; })
            names);

    in
    {
      packages =
        mapAttrNames
          (system: import ./default.nix { inherit system inputs; })
          systems;
    };
}

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
    rec {
      packages =
        mapAttrNames
          (system: import inputs.nixpkgs { inherit system overlays; })
          systems;

      overlays = [
        inputs.emacs-overlay.overlay
        (import ./overlays/kakoune-plugins.nix)
        (import ./overlays/top-level.nix)
        (import ./overlays/envs.nix)
      ];
    };
}

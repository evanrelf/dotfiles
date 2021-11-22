{ pkgs, ... }:

let
  # TODO Do I still need this?
  # declarative-channels =
  #   pkgs.runCommandLocal "declarative-channels" { } ''
  #     mkdir -p $out/channels
  #     ln -s ${pkgs.path} $out/channels/nixpkgs
  #   '';

in
{
  home.packages = with pkgs; [
    direnv
    nix-diff
    nix-prefetch-git
    nix-top
    nix-tree
    nixpkgs-fmt
    # TODO Get Rosetta 2 stuff working
    # rosetta.nix-index
  ];

  xdg.configFile."nix" = {
    source = ../../configs/nix/.config/nix;
    recursive = true;
  };

  nixpkgs = {
    config = { };
    overlays = [
      # TODO Thread Emacs overlay into this module from `flake.nix`
      # inputs.emacs-overlay.overlay
      (import ../../configs/nix/.config/nix/overlays/kakoune-plugins.nix)
      (import ../../configs/nix/.config/nix/overlays/top-level.nix)
      (import ../../configs/nix/.config/nix/overlays/envs.nix)
    ];
  };
}

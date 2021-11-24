{ inputs, pkgs, ... }:

let
  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${inputs.nixpkgs} $out/channels/nixpkgs
    '';

in
{
  home.packages = with pkgs; [
    declarative-channels
    direnv
    nix-diff
    nix-index
    nix-prefetch-git
    nix-top
    nix-tree
    nixpkgs-fmt
  ];
}

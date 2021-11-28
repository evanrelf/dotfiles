{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.nix;

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${inputs.nixpkgs} $out/channels/nixpkgs
    '';

in
{
  options = {
    dotfiles.programs.nix = {
      enable = lib.mkEnableOption "nix";
    };
  };

  config = lib.mkIf cfg.enable {
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
  };
}

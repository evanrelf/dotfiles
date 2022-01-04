{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.dotfiles.programs.nix;

  extra-substituters = lib.concatStringsSep " " [
    "https://evanrelf.cachix.org/"
    "https://nix-community.cachix.org"
  ];

  extra-trusted-public-keys = lib.concatStringsSep " " [
    "evanrelf.cachix.org-1:n9mrgldEeLIlie/UEGulvshb2Yf5bxz1ZYUIvV5kdO4="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];

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
      nix-direnv
      nix-index
      nix-top
      nix-tree
      nixpkgs-fmt
    ];

    xdg.configFile."nix/nix.conf".text = lib.mkForce ''
      extra-substituters = ${extra-substituters}
      extra-trusted-public-keys = ${extra-trusted-public-keys}
      extra-experimental-features = nix-command flakes
    '';

    xdg.configFile."direnv/direnvrc".text = ''
      source "${pkgs.nix-direnv}/share/nix-direnv/direnvrc"
    '';
  };
}

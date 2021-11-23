{ inputs, pkgs, ... }:

let
  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${inputs.nixpkgs} $out/channels/nixpkgs
    '';

  # TODO
  # rosetta =
  #   import pkgsFinal.path {
  #     system =
  #       if pkgsFinal.system == "aarch64-darwin" then
  #         "x86_64-darwin"
  #       else
  #         pkgsFinal.system;

  #     inherit (pkgsFinal) overlays;
  #   };

in
{
  home.packages = with pkgs; [
    declarative-channels
    direnv
    nix-diff
    nix-prefetch-git
    nix-top
    nix-tree
    nixpkgs-fmt
    # TODO Get Rosetta 2 stuff working
    # rosetta.nix-index
  ];

  xdg.configFile."nixpkgs/overlays" = {
    source = ../../overlays;
    recursive = true;
  };
}

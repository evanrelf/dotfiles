let
  pkgs = import ./nix/.config/nix/pkgs.nix;

in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      racket-minimal
      stow
    ];
  }

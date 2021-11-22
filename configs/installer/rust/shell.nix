let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.mkShell {
  inputsFrom = [
    (import ./default.nix)
  ];

  buildInputs = [
    pkgs.rustfmt
  ];
}

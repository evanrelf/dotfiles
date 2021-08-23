let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.stdenv.mkDerivation {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  nativeBuildInputs = [ pkgs.makeWrapper pkgs.zig ];

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    zig build -Drelease-safe -Dcpu=baseline --prefix $out install
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

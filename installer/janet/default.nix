let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.stdenv.mkDerivation {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  buildInputs = [ pkgs.janet ];

  buildPhase = ''
    false # TODO
  '';

  postInstall = ''
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

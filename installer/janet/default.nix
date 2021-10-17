let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.stdenv.mkDerivation {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  buildInputs = [ pkgs.janet ];

  # TODO
  installPhase = ''
    mkdir -p $out/bin/
    echo exec ${pkgs.janet}/bin/janet "$src/src/main.janet" '$@' > $out/bin/installer
    chmod +x $out/bin/installer
  '';

  postInstall = ''
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

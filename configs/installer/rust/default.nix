let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.rustPlatform.buildRustPackage {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  cargoSha256 = "1aksax3xz0pqak9gni1xz5rvfcnbg30a66px4vylbqavf57dn2ff";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

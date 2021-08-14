let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.rustPlatform.buildRustPackage {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  cargoSha256 = "0j9xpwrjdvb4mhipvf9kxjnan6kmc06rk91qxv6nz9zl4jjkxv8p";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
pkgs.buildGoModule {
  name = "installer";

  src = pkgs.gitignoreSource ./.;

  vendorSha256 = "1j2lx12ma39jjfi33irjq4n3pzn69y674ma7h4xz8xvq31d98d8i";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  postInstall = ''
    mv $out/bin/{go,installer}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
}

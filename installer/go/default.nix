let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

  goModule = pkgs.buildGoModule rec {
    name = "installer";
    src = pkgs.gitignoreSource ./.;
    vendorSha256 = "1j2lx12ma39jjfi33irjq4n3pzn69y674ma7h4xz8xvq31d98d8i";
    postInstall = ''
      mv $out/bin/{go,${name}}
    '';
  };

in
goModule.overrideAttrs (old: {
  buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${old.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

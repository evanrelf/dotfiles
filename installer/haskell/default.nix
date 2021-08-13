let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

  haskellPackage =
    pkgs.haskellPackages.callCabal2nix
      "installer"
      (pkgs.gitignoreSource ./.)
      { };

in
haskellPackage.overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${old.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

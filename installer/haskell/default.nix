let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
(pkgs.haskellPackages.callCabal2nix
  "installer"
  (pkgs.gitignoreSource ./.)
  { }
).overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${old.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

in
(pkgs.haskellPackages.callCabal2nix
  "installer"
  (pkgs.gitignoreSource ./.)
  { }
).overrideAttrs (prev: {
  nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${prev.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

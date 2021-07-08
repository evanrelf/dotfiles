let
  pkgs = import ../nix/.config/nix/pkgs.nix { };

  haskellPackage = pkgs.haskellPackages.callCabal2nix "installer" ./. { };

in
pkgs.haskell.lib.overrideCabal haskellPackage (old: {
  buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${old.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

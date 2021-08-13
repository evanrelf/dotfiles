let
  pkgs = import ../../nix/.config/nix/pkgs.nix { };

  gitignoreSource =
    let
      source = pkgs.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
        sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
      };
    in
    (import source { inherit (pkgs) lib; }).gitignoreSource;

  haskellPackage =
    pkgs.haskellPackages.callCabal2nix
      "installer"
      (gitignoreSource ./.)
      { };

in
pkgs.haskell.lib.overrideCabal haskellPackage (old: {
  buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    ${old.postInstall or ""}
    wrapProgram $out/bin/installer --prefix PATH : ${pkgs.stow}/bin
  '';
})

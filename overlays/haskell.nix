pkgsFinal: pkgsPrev:

{
  haskellScript =
    pkgsFinal.callPackage ../src/nix/haskell-script.nix { };

  haskell-hello =
    pkgsFinal.haskellScript {
      name = "haskell-hello";
      packages = p: [ p.relude ];
      script = ../src/haskell/hello.hs;
    };
}

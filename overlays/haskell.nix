pkgsFinal: pkgsPrev:

{
  haskellScript =
    pkgsFinal.callPackage ../src/nix/haskell-script.nix { };

  hello-haskell =
    pkgsFinal.haskellScript {
      name = "hello-haskell";
      packages = p: [ p.relude ];
      script = ../src/haskell/hello.hs;
    };
}

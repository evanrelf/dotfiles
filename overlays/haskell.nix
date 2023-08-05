pkgsFinal: pkgsPrev:

{
  haskellScript =
    pkgsFinal.callPackage ../src/nix/haskell-script.nix { };

  memorize-haskell =
    pkgsFinal.haskellScript {
      name = "memorize-haskell";
      packages = p: [
        p.ansi-terminal
        p.optparse-applicative
        p.random
        p.relude
        p.witch
      ];
      script = ../src/haskell/memorize.hs;
    };

  hello-haskell =
    pkgsFinal.haskellScript {
      name = "hello-haskell";
      packages = p: [ p.relude ];
      script = ../src/haskell/hello.hs;
    };
}

final: prev:

{
  haskellScript =
    final.callPackage ../src/nix/haskell-script.nix { };

  hello-haskell =
    final.haskellScript {
      name = "hello-haskell";
      packages = p: [ p.relude ];
      script = ../src/haskell/hello.hs;
    };
}

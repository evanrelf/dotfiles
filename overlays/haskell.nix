pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev) haskell-overlay;

in
haskell-overlay.mkOverlay
{
  extensions = [
    (haskell-overlay.override (haskellPackagesFinal: haskellPackagesPrev: {
      ghc-tags = prev: {
        ghc-lib = haskellPackagesFinal.ghc-lib_9_2_4_20220729.override (prev: {
          ghc-lib-parser = haskellPackagesFinal.ghc-lib-parser_9_2_4_20220729;
        });
      };
    }))

    (haskell-overlay.overrideCabal (haskellPackagesFinal: haskellPackagesPrev: {
      ghc-tags = prev: {
        broken = false;
      };
    }))
  ];
}
  pkgsFinal
  pkgsPrev

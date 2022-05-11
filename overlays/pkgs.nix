pkgsFinal: pkgsPrev:

{
  comma-update =
    pkgsPrev.naersk.buildPackage {
      root = ../src;
      src = ../src/comma-update;
    };

  home-rebuild =
    pkgsPrev.naersk.buildPackage {
      root = ../src;
      src = ../src/home-rebuild;
    };
}

pkgsFinal: pkgsPrev:

{
  comma-update =
    pkgsFinal.crane.buildPackage {
      src = ../src/rust/comma-update;
      nativeBuildInputs = [
        pkgsFinal.darwin.apple_sdk.frameworks.Security
      ];
    };

  home-rebuild =
    pkgsFinal.crane.buildPackage {
      src = ../src/rust/home-rebuild;
    };

  memorize-rust =
    pkgsFinal.crane.buildPackage {
      src = ../src/rust/memorize;
      nativeBuildInputs = [
        pkgsFinal.libiconv
      ];
    };
}

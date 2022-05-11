pkgsFinal: pkgsPrev:

let
  src = ../src;

  cargoArtifacts =
    pkgsFinal.crane.buildDepsOnly { inherit src; };

in
{
  comma-update =
    pkgsFinal.crane.buildPackage {
      inherit src cargoArtifacts;
      cargoBuildCommand = "cargo build --release --bin comma-update";
    };

  home-rebuild =
    pkgsFinal.crane.buildPackage {
      inherit src cargoArtifacts;
      cargoBuildCommand = "cargo build --release --bin home-rebuild";
    };

  crane = pkgsPrev.crane.overrideScope' (craneFinal: cranePrev: {
    mkCargoDerivation = args:
      (pkgsPrev.crane.mkCargoDerivation args).overrideAttrs (prev: {
        nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++
          pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
            pkgsFinal.darwin.apple_sdk.frameworks.Security
            pkgsFinal.libiconv
          ];
      });
  });
}

pkgsFinal: pkgsPrev:

let
  src = ../src/rust;

  crane =
    pkgsPrev.crane.overrideScope' (craneFinal: cranePrev: {
      mkCargoDerivation = args:
        (pkgsPrev.crane.mkCargoDerivation args).overrideAttrs (prev: {
          nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++
            pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
              pkgsFinal.darwin.apple_sdk.frameworks.Security
              pkgsFinal.libiconv
            ];
        });
    });

  cargoArtifacts =
    crane.buildDepsOnly {
      pname = "cargoArtifacts";
      version = "0.0.0";
      inherit src;
    };

in
{
  comma-update =
    crane.buildPackage {
      pname = "comma-update";
      version = "0.0.0";
      inherit src cargoArtifacts;
      cargoBuildCommand = "cargo build --release --bin comma-update";
    };

  home-rebuild =
    crane.buildPackage {
      pname = "home-rebuild";
      version = "0.0.0";
      inherit src cargoArtifacts;
      cargoBuildCommand = "cargo build --release --bin home-rebuild";
    };
}

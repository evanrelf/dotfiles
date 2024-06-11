final: prev:

let
  inherit (final.evan.lib) checkVersion;

  gprefix = drv:
    final.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in
{
  coreutils-gprefix =
    (prev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (attrs: {
      doCheck = false;
    });

  findutils-gprefix =
    gprefix final.findutils;

  gawkInteractive-gprefix =
    gprefix final.gawkInteractive;

  ghciwatch =
    let version = "0.5.13"; in
    (checkVersion version prev.ghciwatch).overrideAttrs (attrs: rec {
      inherit version;
      src = final.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = "ghciwatch";
        rev = "release/${version}";
        hash = "sha256-579dUVfbakhXH/m+wiZRmmYzUHa/mB4pJ49rUA9iAGw=";
      };
      cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
        name = "${attrs.pname}-${version}-vendor.tar.gz";
        inherit src;
        outputHash = "sha256-/wjTZB0JKaAVwjdAl7JbiYSvR/fkvG7Todqxj9YAmAs=";
      });
    });

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  jujutsu =
    let version = "0.18.0"; in
    (checkVersion version prev.jujutsu).overrideAttrs (attrs: rec {
      inherit version;
      src = final.fetchFromGitHub {
        owner = "martinvonz";
        repo = "jj";
        rev = "v${version}";
        hash = "sha256-5KKF85RNCPPaXMxBb7m2XC3EaEo+UcEhBdfMEzNPsAg=";
      };
      cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
        name = "${attrs.pname}-${version}-vendor.tar.gz";
        inherit src;
        outputHash = "sha256-MiJuen3Lo7nPaAK30cENw3ACAdoYbHDoiGS05dk5m6U=";
      });
    });

  kakoune-unwrapped =
    prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
      version = final.inputs.kakoune.shortRev;
      src = final.inputs.kakoune;
      patches = [ ];
      preConfigure = ''
        ${attrs.preConfigure or ""}
        export version="${version}"
      '';
    });

  roc =
    final.inputs'.roc.packages.cli.override (inputs: {
      compile-deps = (inputs.compile-deps or { }) // {
        zigPkg =
          assert inputs.compile-deps.zigPkg.version == "0.11.0";
          final.zigpkgs."0.11.0";
      };
    });
}

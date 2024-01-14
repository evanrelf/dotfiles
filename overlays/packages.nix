final: prev:

let
  inherit (final.evan) checkVersion;

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
    assert !(prev ? ghciwatch);
    final.crane.buildPackage rec {
      pname = "ghciwatch";
      version = "0.5.3";
      src = final.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = pname;
        rev = "v${version}";
        hash = "sha256-tPSrNFo0B/LPsaeOqT/jVu6HOOzoGxx+D0USnaJ5wxI=";
      };
      buildInputs =
        final.lib.optionals final.stdenv.isDarwin [
          final.darwin.apple_sdk.frameworks.CoreServices
          final.libiconv
        ];
      doCheck = false; # Workaround for missing `GHC_VERSIONS`
    };

  gnugrep-gprefix =
    gprefix final.gnugrep;

  gnused-gprefix =
    gprefix final.gnused;

  graphex =
    assert !(prev ? graphex);
    let
      src = final.fetchFromGitHub {
        owner = "dustin";
        repo = "graphex";
        rev = "d330549e36c833b1bab4fce5f77838196682a925";
        hash = "sha256-apgpqPnKXX6giPEqucJMpVXLzSDKYVB4PfNp2Kw1Y/0=";
      };
    in
    final.haskellPackages.callCabal2nix "graphex" src { };

  # jujutsu =
  #   let version = "0.11.0"; in
  #   (checkVersion version prev.jujutsu).overrideAttrs (attrs: rec {
  #     inherit version;
  #     src = final.fetchFromGitHub {
  #       owner = "martinvonz";
  #       repo = "jj";
  #       rev = "v${version}";
  #       hash = "sha256-yEW7+0MnJlW0WeZ6UItaCDrihPLA52mLcu15tJwZx9w=";
  #     };
  #     cargoDeps = attrs.cargoDeps.overrideAttrs (final.lib.const {
  #       name = "${attrs.pname}-${version}-vendor.tar.gz";
  #       inherit src;
  #       outputHash = "sha256-xA9SDq1Kc0u8qFEPFFCic9uwE2Y/BXJzUHBCs1Czxtw=";
  #     });
  #   });

  # kakoune-unwrapped =
  #   prev.kakoune-unwrapped.overrideAttrs (attrs: rec {
  #     version = "HEAD";
  #     src = final.inputs.kakoune;
  #     preConfigure = ''
  #       ${attrs.preConfigure}
  #       export version="${version}"
  #     '';
  #   });

  qsv =
    assert !(prev ? qsv);
    let
      pname = "qsv";
      version = "0.118.0";
      src = final.fetchFromGitHub {
        owner = "jqnatividad";
        repo = pname;
        rev = version;
        hash = "sha256-EVNqWETlKO7bpnh3rn6wjntgk5Xqa3/mnsu+hxu2UKk=";
      };
      # `all_features` minus `to_parquet`, to avoid the `duckdb` dependency
      # (it's failing to compile and I can't figure out how to fix it)
      features =
        final.lib.pipe src [
          (src: final.lib.importTOML "${src}/Cargo.toml")
          (toml: toml.features.all_features)
          (fs: final.lib.filter (f: f != "to_parquet") fs)
          (fs: final.lib.concatStringsSep "," fs)
        ];
      crane =
        final.crane.overrideToolchain final.fenix.minimal.toolchain;
    in
    crane.buildPackage rec {
      inherit pname version src;
      buildInputs = [
        final.python3
      ] ++ final.lib.optionals final.stdenv.isDarwin [
        final.darwin.apple_sdk.frameworks.Security
        final.darwin.apple_sdk.frameworks.SystemConfiguration
        final.libiconv
      ];
      cargoExtraArgs = "--bin ${pname} --features ${features}";
    };
}

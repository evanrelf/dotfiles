pkgsFinal: pkgsPrev:

let
  inherit (pkgsFinal.evan) checkVersion;

  gprefix = drv:
    pkgsFinal.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in
{
  aichat =
    let version = "0.9.0-d40f104"; in
    (checkVersion version pkgsPrev.aichat).overrideAttrs (attrs: rec {
      inherit version;
      src = pkgsFinal.fetchFromGitHub {
        owner = "sigoden";
        repo = "aichat";
        rev = "d40f104f667073f320a01d9c1a91aa88225ccaeb";
        hash = "sha256-tSMZqm0QfeBJ9S9u9eBCso5Zj/8hf7+VwHMb2gstTOc=";
      };
      buildInputs =
        pkgsFinal.lib.optionals pkgsFinal.stdenv.isDarwin [
          pkgsFinal.darwin.apple_sdk.frameworks.AppKit
          pkgsFinal.libiconv
        ];
      cargoDeps = attrs.cargoDeps.overrideAttrs (pkgsFinal.lib.const {
        name = "${attrs.pname}-${version}-vendor.tar.gz";
        inherit src;
        outputHash = "sha256-KWAhzmUQJ6vq0atjySOPD9im7cmQdO6rOZ5ItCLljlI=";
      });
    });

  calligraphy =
    assert !(pkgsPrev ? calligraphy);
    let
      src = pkgsFinal.fetchFromGitHub {
        owner = "jonascarpay";
        repo = "calligraphy";
        rev = "309a24bc78836de32a86e1c185b7a1c5698ef368";
        hash = "sha256-8B/e3QggX5xtGkVHGq3gYncL+VvpbZpcRb/OBVbznyE=";
      };
    in
    pkgsFinal.haskellPackages.callCabal2nix "calligraphy" src { };

  coreutils-gprefix =
    (pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (prev: {
      doCheck = false;
    });

  emacs =
    pkgsPrev.emacs.pkgs.withPackages (p: [ p.vterm ]);

  findutils-gprefix =
    gprefix pkgsFinal.findutils;

  gawkInteractive-gprefix =
    gprefix pkgsFinal.gawkInteractive;

  ghciwatch =
    assert !(pkgsPrev ? ghciwatch);
    pkgsFinal.crane.buildPackage rec {
      pname = "ghciwatch";
      version = "0.3.8";
      src = pkgsFinal.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = pname;
        rev = "v${version}";
        hash = "sha256-e2GV26yiOHChv9linNnv4MeJxFhv6eljNllQ9TpwLh0=";
      };
      buildInputs =
        pkgsFinal.lib.optionals pkgsFinal.stdenv.isDarwin [
          pkgsFinal.darwin.apple_sdk.frameworks.CoreServices
          pkgsFinal.libiconv
        ];
      doCheck = false; # Workaround for missing `GHC_VERSIONS`
    };

  gnugrep-gprefix =
    gprefix pkgsFinal.gnugrep;

  gnused-gprefix =
    gprefix pkgsFinal.gnused;

  graphex =
    assert !(pkgsPrev ? graphex);
    let
      src = pkgsFinal.fetchFromGitHub {
        owner = "dustin";
        repo = "graphex";
        rev = "d330549e36c833b1bab4fce5f77838196682a925";
        hash = "sha256-apgpqPnKXX6giPEqucJMpVXLzSDKYVB4PfNp2Kw1Y/0=";
      };
    in
    pkgsFinal.haskellPackages.callCabal2nix "graphex" src { };

  jujutsu =
    let version = "0.11.0"; in
    (checkVersion version pkgsPrev.jujutsu).overrideAttrs (attrs: rec {
      inherit version;
      src = pkgsFinal.fetchFromGitHub {
        owner = "martinvonz";
        repo = "jj";
        rev = "v${version}";
        hash = "sha256-yEW7+0MnJlW0WeZ6UItaCDrihPLA52mLcu15tJwZx9w=";
      };
      cargoDeps = attrs.cargoDeps.overrideAttrs (pkgsFinal.lib.const {
        name = "${attrs.pname}-${version}-vendor.tar.gz";
        inherit src;
        outputHash = "sha256-xA9SDq1Kc0u8qFEPFFCic9uwE2Y/BXJzUHBCs1Czxtw=";
      });
    });

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (prev: rec {
      version = "HEAD";
      src = pkgsFinal.inputs.kakoune;
      preConfigure = ''
        ${prev.preConfigure}
        export version="${version}"
      '';
    });

  qsv =
    assert !(pkgsPrev ? qsv);
    let
      pname = "qsv";
      version = "0.118.0";
      src = pkgsFinal.fetchFromGitHub {
        owner = "jqnatividad";
        repo = pname;
        rev = version;
        hash = "sha256-EVNqWETlKO7bpnh3rn6wjntgk5Xqa3/mnsu+hxu2UKk=";
      };
      # `all_features` minus `to_parquet`, to avoid the `duckdb` dependency
      # (it's failing to compile and I can't figure out how to fix it)
      features =
        pkgsFinal.lib.pipe src [
          (src: pkgsFinal.lib.importTOML "${src}/Cargo.toml")
          (toml: toml.features.all_features)
          (fs: pkgsFinal.lib.filter (f: f != "to_parquet") fs)
          (fs: pkgsFinal.lib.concatStringsSep "," fs)
        ];
      crane =
        pkgsFinal.crane.overrideToolchain pkgsFinal.fenix.minimal.toolchain;
    in
    crane.buildPackage rec {
      inherit pname version src;
      buildInputs = [
        pkgsFinal.python3
      ] ++ pkgsFinal.lib.optionals pkgsFinal.stdenv.isDarwin [
        pkgsFinal.darwin.apple_sdk.frameworks.Security
        pkgsFinal.darwin.apple_sdk.frameworks.SystemConfiguration
        pkgsFinal.libiconv
      ];
      cargoExtraArgs = "--bin ${pname} --features ${features}";
    };
}

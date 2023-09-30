pkgsFinal: pkgsPrev:

let
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
  calligraphy =
    let
      src = pkgsFinal.fetchFromGitHub {
        owner = "jonascarpay";
        repo = "calligraphy";
        rev = "309a24bc78836de32a86e1c185b7a1c5698ef368";
        sha256 = "sha256-8B/e3QggX5xtGkVHGq3gYncL+VvpbZpcRb/OBVbznyE=";
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

  ghcid-ng =
    pkgsFinal.crane.buildPackage rec {
      pname = "ghcid-ng";
      version = "0.3.4";
      src = pkgsFinal.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = "ghcid-ng";
        rev = "v${version}";
        sha256 = "sha256-nRRX2qRXCxhIkbCrxvI/K5VGdWp3a7Ca5Lkt4TmE5u8=";
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
    let
      src = pkgsFinal.fetchFromGitHub {
        owner = "dustin";
        repo = "graphex";
        rev = "d330549e36c833b1bab4fce5f77838196682a925";
        sha256 = "sha256-apgpqPnKXX6giPEqucJMpVXLzSDKYVB4PfNp2Kw1Y/0=";
      };
    in
    pkgsFinal.haskellPackages.callCabal2nix "graphex" src { };

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
    let
      crane =
        pkgsFinal.crane.overrideToolchain pkgsFinal.fenix.minimal.toolchain;
    in
    crane.buildPackage rec {
      pname = "qsv";
      version = "0.74.0";
      src = pkgsFinal.fetchFromGitHub {
        owner = "jqnatividad";
        repo = "qsv";
        rev = version;
        hash = "sha256-zMxvA/dc1MoLn7z7y/yWKBc+cYCHI0MO0tiLMNcBKeY=";
      };
      buildInputs = [
        pkgsFinal.python3
      ] ++ pkgsFinal.lib.optionals pkgsFinal.stdenv.isDarwin [
        pkgsFinal.darwin.apple_sdk.frameworks.Security
        pkgsFinal.libiconv
      ];
      cargoExtraArgs = "--features all_full";
    };

  scm-diff-editor =
    pkgsFinal.crane.buildPackage {
      pname = "scm-diff-editor";
      version = "0.0.0";
      src = pkgsFinal.fetchFromGitHub {
        owner = "arxanas";
        repo = "git-branchless";
        rev = "8520d4e1b00055e8f927129e2a4cafda35aedf32";
        hash = "sha256-j3mBZ2adbNMhUn39hD50YFsAKAFh889FGv0BstQ0x4k=";
      };
      buildInputs = pkgsFinal.lib.optionals pkgsFinal.stdenv.isDarwin [
        pkgsFinal.libiconv
      ];
      cargoExtraArgs = "--package scm-record --features scm-diff-editor";
    };
}

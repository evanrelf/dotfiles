pkgsFinal: pkgsPrev:

let
  gprefix = drv:
    pkgsPrev.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in
{
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
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = "ghcid-ng";
        rev = "1a33f8853215cf95018b172e169600a4e57f012b";
        sha256 = "sha256-HK5s0GgfQ5vI1XCa1nx9DC5qneb9ozSt5/bQRwLoHHM=";
      };
      buildInputs =
        pkgsFinal.lib.optionals pkgsPrev.stdenv.isDarwin [
          pkgsFinal.darwin.apple_sdk.frameworks.CoreServices
          pkgsFinal.libiconv
        ];
    };

  gnugrep-gprefix =
    gprefix pkgsFinal.gnugrep;

  gnused-gprefix =
    gprefix pkgsFinal.gnused;

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (prev: rec {
      version = "HEAD";
      src = pkgsPrev.inputs.kakoune;
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
      src = pkgsPrev.fetchFromGitHub {
        owner = "jqnatividad";
        repo = "qsv";
        rev = version;
        hash = "sha256-zMxvA/dc1MoLn7z7y/yWKBc+cYCHI0MO0tiLMNcBKeY=";
      };
      buildInputs = [
        pkgsFinal.python3
      ] ++ pkgsFinal.lib.optionals pkgsPrev.stdenv.isDarwin [
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
      buildInputs = pkgsFinal.lib.optionals pkgsPrev.stdenv.isDarwin [
        pkgsFinal.libiconv
      ];
      cargoExtraArgs = "--package scm-record --features scm-diff-editor";
    };
}

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

  ghciwatch =
    pkgsFinal.crane.buildPackage rec {
      pname = "ghciwatch";
      version = "0.3.8";
      src = pkgsFinal.fetchFromGitHub {
        owner = "MercuryTechnologies";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-e2GV26yiOHChv9linNnv4MeJxFhv6eljNllQ9TpwLh0=";
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
}

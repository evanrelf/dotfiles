pkgsFinal: pkgsPrev:

let
  gprefix = drv:
    pkgsPrev.runCommandLocal "gprefix-${drv.name}" {} ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in {
  cabal-plan =
    pkgsPrev.haskell.lib.justStaticExecutables
      (pkgsPrev.haskell.lib.overrideCabal
        pkgsPrev.haskellPackages.cabal-plan
        (oldCabal: {
          configureFlags =
            (oldCabal.configureFlags or []) ++ [ "-flicense-report" ];
          executableHaskellDepends =
            (oldCabal.executableHaskellDepends or [])
              ++ (with pkgsFinal.haskellPackages; [ tar zlib ]);
        }) );

  comma =
    import (pkgsPrev.fetchFromGitHub {
      owner = "shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
    }) { pkgs = pkgsFinal; };

  coreutils-gprefix =
    pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };

  dhall =
    let
      version = "1.37.1";
      fetch = { system, sha256 }:
        pkgsPrev.fetchzip {
          name = "dhall-${version}";
          url = "https://github.com/dhall-lang/dhall-haskell/releases/download/${version}/dhall-${version}-x86_64-${system}.tar.bz2";
          inherit sha256;
          stripRoot = false;
        };
    in
      if pkgsPrev.stdenv.isDarwin then
        fetch {
          system = "macos";
          sha256 = "1hzzrrc7pqqxnm9abc5ixlfnsxi7rnszwdbgk9v75k0wz52jr1rk";
        }
      else
        fetch {
          system = "linux";
          sha256 = "04ivmcxw7zzxr3q5svwg29y37wh60qkzvlan71k52hr12k4qnls9";
        };

  emacsGccVterm =
    (pkgsPrev.emacsPackagesGen pkgsFinal.emacsGcc).emacsWithPackages (p: [ p.vterm ]);

  evanPackages = {
    runghc =
      let
        ghc = pkgsFinal.haskellPackages.ghcWithPackages (p: with p; [
          relude
          string-interpolate
          unliftio
        ]);
      in
        pkgsPrev.runCommandLocal "evans-runghc" {
          buildInputs = [ pkgsFinal.makeWrapper ];
        } ''
          mkdir -p "$out/bin"
          makeWrapper "${ghc}/bin/runghc" "$out/bin/evans-runghc" \
            --add-flags "--ghc-arg='-XBlockArguments'" \
            --add-flags "--ghc-arg='-XDeriveAnyClass'" \
            --add-flags "--ghc-arg='-XDeriveFunctor'" \
            --add-flags "--ghc-arg='-XDeriveGeneric'" \
            --add-flags "--ghc-arg='-XDerivingStrategies'" \
            --add-flags "--ghc-arg='-XDerivingVia'" \
            --add-flags "--ghc-arg='-XGeneralizedNewtypeDeriving'" \
            --add-flags "--ghc-arg='-XInstanceSigs'" \
            --add-flags "--ghc-arg='-XLambdaCase'" \
            --add-flags "--ghc-arg='-XNamedFieldPuns'" \
            --add-flags "--ghc-arg='-XNoImplicitPrelude'" \
            --add-flags "--ghc-arg='-XOverloadedStrings'" \
            --add-flags "--ghc-arg='-XScopedTypeVariables'" \
            --add-flags "--ghc-arg='-XTypeApplications'" \
            --add-flags "--ghc-arg='-Wall'" \
            --add-flags "--ghc-arg='-Wcompat'" \
            --add-flags "--ghc-arg='-Werror=incomplete-record-updates'" \
            --add-flags "--ghc-arg='-Werror=incomplete-uni-patterns'" \
            --add-flags "--ghc-arg='-Werror=missing-fields'" \
            --add-flags "--ghc-arg='-Werror=partial-fields'" \
            --add-flags "--ghc-arg='-Widentities'" \
            --add-flags "--ghc-arg='-Wmissing-home-modules'" \
            --add-flags "--ghc-arg='-Wredundant-constraints'" \
            --add-flags "--ghc-arg='-foptimal-applicative-do'" \
            --add-flags "--ghc-arg='-fshow-warning-groups'" \
            --add-flags "--ghc-arg='-threaded'" \
            --add-flags "--ghc-arg='-rtsopts'" \
            --add-flags "--ghc-arg='-with-rtsopts=-N'"
        '';
  };

  findutils-gprefix = gprefix pkgsFinal.findutils;

  fourmolu =
    pkgsPrev.haskell.lib.justStaticExecutables
      (pkgsPrev.haskell.lib.doJailbreak
        (pkgsPrev.haskellPackages.callCabal2nix
          "fourmolu"
          (pkgsPrev.fetchFromGitHub {
            owner = "parsonsmatt";
            repo = "fourmolu";
            rev = "45a8478b8e6ba48b4ce228d4aaee3cb9f5aa08f6"; # 0.3.0.0
            sha256 = "0w4m887pr2ad303a35dl9gs03xza2fy6mnbgl65s0yal78mfw0zv";
          })
          {}));

  gawk-gprefix = gprefix pkgsFinal.gawk;

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  iosevka-pro =
    # To install on macOS:
    # $ open $(nix-env --query env --out-path | awk '{print $2}')"/share/fonts/iosevka-pro/"*
    pkgsPrev.iosevka.override {
      set = "pro";
      privateBuildPlan = {
        family = "Iosevka Pro";
        design = [
          # PragmataPro style
          "ss08"
          # Make "Term" variant
          "sp-term"
          # Add Haskell ligatures
          "ligset-haskell"
          # Add != and !== ligatures
          "calt-exeq"
          # Add <!-- and <!--- ligatures
          "calt-html-comment"
        ];
      };
    };

  kakoune =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "df7b33bc7b9da897d1d0127b54ae1e629d2334ff";
        sha256 = "1jjk9zbxxdn8k62sbg7smvwvjskrj0k4ki03xbc0dq9jjhvz2f46";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  man-gprefix = gprefix pkgsFinal.man;

  nix-tree =
    (import (pkgsPrev.fetchFromGitHub {
      owner = "utdemir";
      repo = "nix-tree";
      rev = "8f32ee74f58bfba454e33a8459276795191be364";
      sha256 = "0m7fdxm1zl7p48dppxxg932fv3pc7idxi4jrp5qf66jccg6ihwhl";
    })).defaultPackage."${builtins.currentSystem}";

  teip =
    pkgsPrev.rustPlatform.buildRustPackage rec {
      pname = "teip";
      version = "1.2.1";
      src = pkgsPrev.fetchFromGitHub {
        owner = "greymd";
        repo = "teip";
        rev = "v${version}";
        sha256 = "0s6ks6vdjrqri9cp48zgddjhmap9l81mygyrz6pa7xhcs48zaj23";
      };
      cargoPatches = [ ./teip-cargo-lock.patch ];
      cargoSha256 = "1mk00s1yfrgw4q1rsl81b45bcv1gk216czvb4xvm8f5d4shk17l7";
      doCheck = false;
    };
}

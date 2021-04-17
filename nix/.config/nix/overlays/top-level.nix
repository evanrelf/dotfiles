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
  comma =
    import
      (pkgsPrev.fetchFromGitHub {
        owner = "shopify";
        repo = "comma";
        rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
        sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
      })
      { pkgs = pkgsFinal; };

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
      fetch
        {
          system = "macos";
          sha256 = "1hzzrrc7pqqxnm9abc5ixlfnsxi7rnszwdbgk9v75k0wz52jr1rk";
        }
    else
      fetch {
        system = "linux";
        sha256 = "04ivmcxw7zzxr3q5svwg29y37wh60qkzvlan71k52hr12k4qnls9";
      };

  evan-runghc =
    let
      ghc = pkgsFinal.haskellPackages.ghcWithPackages (p: with p; [
        relude
        string-interpolate
        unliftio
      ]);

      ghcArgs = builtins.map (arg: ''--add-flags "--ghc-arg='${arg}'"'') [
        "-XBlockArguments"
        "-XDeriveAnyClass"
        "-XDeriveFunctor"
        "-XDeriveGeneric"
        "-XDerivingStrategies"
        "-XDerivingVia"
        "-XGeneralizedNewtypeDeriving"
        "-XInstanceSigs"
        "-XLambdaCase"
        "-XNamedFieldPuns"
        "-XNoImplicitPrelude"
        "-XOverloadedStrings"
        "-XScopedTypeVariables"
        "-XTypeApplications"
        "-Wall"
        "-Wcompat"
        "-Werror=incomplete-record-updates"
        "-Werror=incomplete-uni-patterns"
        "-Werror=missing-fields"
        "-Werror=partial-fields"
        "-Widentities"
        "-Wmissing-home-modules"
        "-Wredundant-constraints"
        "-foptimal-applicative-do"
        "-fshow-warning-groups"
        "-threaded"
        "-rtsopts"
        "-with-rtsopts=-N"
      ];
    in
    pkgsPrev.runCommandLocal "evan-runghc"
      {
        buildInputs = [ pkgsFinal.makeWrapper ];
      } ''
      mkdir -p "$out/bin"
      makeWrapper "${ghc}/bin/runghc" "$out/bin/evans-runghc" ${ghcArgs}
    '';

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
          { }));

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "545db22ae464668d36fad500690a77c019936910";
        sha256 = "08v31b70v815sjfzzz9jl3flk1k0ynv0n4dv68x0rkkkmxvzmnls";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  neovim-unwrapped =
    pkgsPrev.neovim-unwrapped.overrideAttrs (old: {
      version = "0.5.0-dev+1157-g0ab88c2ea";
      src =
        pkgsPrev.fetchFromGitHub {
          owner = "neovim";
          repo = "neovim";
          rev = "0ab88c2ea80caa7cda97b3a8479d0d32e4636ab6";
          sha256 = "07jiv9c3032lrhmd3dvqv2v5l35bdn39jqi48qsjj220slrsrl53";
        };
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgsFinal.tree-sitter ];
    });

  nix-tree =
    (import (pkgsPrev.fetchFromGitHub {
      owner = "utdemir";
      repo = "nix-tree";
      rev = "8f32ee74f58bfba454e33a8459276795191be364";
      sha256 = "0m7fdxm1zl7p48dppxxg932fv3pc7idxi4jrp5qf66jccg6ihwhl";
    })).defaultPackage."${builtins.currentSystem}";
}

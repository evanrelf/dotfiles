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
  # cabal-edit =
  #   (import (pkgsPrev.fetchFromGitHub {
  #     owner = "sdiehl";
  #     repo = "cabal-edit";
  #     rev = "0000000000000000000000000000000000000000";
  #     sha256 = "0000000000000000000000000000000000000000000000000000";
  #   }) { pkgs = pkgsFinal; });

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

  ghcide =
    (import (pkgsPrev.fetchFromGitHub {
      owner = "cachix";
      repo = "ghcide-nix";
      rev = "67493b873e1a5e5d53837401ab6b128b20e8a989";
      sha256 = "1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
    }) {}).ghcide-ghc865;

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  headroom =
    pkgsPrev.haskell.lib.justStaticExecutables
      (pkgsPrev.haskellPackages.callCabal2nix
        "headroom"
        (pkgsPrev.fetchFromGitHub {
          owner = "vaclavsvejcar";
          repo = "headroom";
          rev = "v0.3.2.0";
          sha256 = "1nk98ng7pfacr0f27z314qf4m3fas8a0nbrax0r0bhjvwzg6i73r";
        })
        {});

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

  kak-lsp =
    pkgsPrev.rustPlatform.buildRustPackage {
      pname = "kak-lsp";
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "kak-lsp";
        repo = "kak-lsp";
        rev = "354b46e3cf56f0da35b444941a701ca4c1135aa8";
        sha256 = "00hwf7pgrhrk0d572xp4k82pama09ph7k8s63cg28ixsmzhpaiji";
      };
      cargoSha256 = "0zn0y68fs4hgr4ypym4rqcr7ipsh3nxxhlrky5n43mp3qkbgyahs";
    };

  kakoune =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "27e95ed6579adf40921bdad66bc4fc7fee94d539";
        sha256 = "1m24i6l4lnigf4w7r55m498vjhqm1nklm4q45101hyzmc10ck7bj";
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

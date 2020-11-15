self: super:

let
  gprefix = drv:
    super.runCommandLocal "gprefix-${drv.name}" {} ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in {
  # cabal-edit =
  #   (import (super.fetchFromGitHub {
  #     owner = "sdiehl";
  #     repo = "cabal-edit";
  #     rev = "0000000000000000000000000000000000000000";
  #     sha256 = "0000000000000000000000000000000000000000000000000000";
  #   }) { pkgs = self; });

  cabal-plan =
    super.haskell.lib.justStaticExecutables
      (super.haskell.lib.overrideCabal super.haskellPackages.cabal-plan (old: {
        configureFlags =
          (old.configureFlags or []) ++ [ "-flicense-report" ];
        executableHaskellDepends =
          (old.executableHaskellDepends or []) ++ (with self.haskellPackages; [ tar zlib ]);
      }));

  comma =
    import (super.fetchFromGitHub {
      owner = "shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
    }) { pkgs = self; };

  coreutils-gprefix =
    super.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };

  emacsGccVterm =
    (super.emacsPackagesGen self.emacsGcc).emacsWithPackages (p: [ p.vterm ]);

  findutils-gprefix = gprefix self.findutils;

  fourmolu =
    super.haskell.lib.justStaticExecutables
      (super.haskell.lib.doJailbreak
        (super.haskellPackages.callCabal2nix
          "fourmolu"
          (super.fetchFromGitHub {
            owner = "parsonsmatt";
            repo = "fourmolu";
            rev = "45a8478b8e6ba48b4ce228d4aaee3cb9f5aa08f6"; # 0.3.0.0
            sha256 = "0w4m887pr2ad303a35dl9gs03xza2fy6mnbgl65s0yal78mfw0zv";
          })
          {}));

  gawk-gprefix = gprefix self.gawk;

  ghcide =
    (import (super.fetchFromGitHub {
      owner = "cachix";
      repo = "ghcide-nix";
      rev = "67493b873e1a5e5d53837401ab6b128b20e8a989";
      sha256 = "1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
    }) {}).ghcide-ghc865;

  gnugrep-gprefix = gprefix self.gnugrep;

  iosevka-pro =
    # To install on macOS:
    # $ open $(nix-env --query env --out-path | awk '{print $2}')"/share/fonts/iosevka-pro/"*
    super.iosevka.override {
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
    super.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = super.fetchFromGitHub {
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

  nix-tree =
    (import (super.fetchFromGitHub {
      owner = "utdemir";
      repo = "nix-tree";
      rev = "c3c77ede124e91c3275a994f9e57cab360e6a13a";
      sha256 = "0nyln9cxybqdcp9pdh5qjhhmbl0bn8f9ghh4cn0q2wmsldqpl0ai";
    })).nix-tree;

  teip =
    super.rustPlatform.buildRustPackage {
      pname = "teip";
      version = "1.2.1";
      src = super.fetchFromGitHub {
        owner = "greymd";
        repo = "teip";
        rev = "52b7a8b4a6cbf354a3620936cec7add257e2f483";
        sha256 = "0s6ks6vdjrqri9cp48zgddjhmap9l81mygyrz6pa7xhcs48zaj23";
      };
      cargoPatches = [ ./teip-cargo-lock.patch ];
      cargoSha256 = "1mk00s1yfrgw4q1rsl81b45bcv1gk216czvb4xvm8f5d4shk17l7";
      doCheck = false;
    };
}

self: super:

{ kakoune =
    super.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = super.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "c585107ab5e7155f7da648c3752cf360f7156177";
        sha256 = "1rjnhkzwrwxkbi78rpbl06d815jdkpkfpfcv5ppclvpwyqfd98zc";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  lorri =
    import (super.fetchFromGitHub {
      owner = "target";
      repo = "lorri";
      rev = "1.1";
      sha256 = "0wbkx8hmikngfp6fp0y65yla22f3k0jszq8a6pas80q0b33llwm5";
    }) { pkgs = self; };

  comma =
    import (super.fetchFromGitHub {
      owner = "shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
    }) { pkgs = self; };

  ormolu =
    super.haskell.lib.justStaticExecutables
      (super.haskellPackages.ormolu_0_1_0_0.override {
        ghc-lib-parser = self.haskellPackages.ghc-lib-parser_8_10_1_20200523;
      });

  ghcide =
    (import (super.fetchFromGitHub {
      owner = "cachix";
      repo = "ghcide-nix";
      rev = "67493b873e1a5e5d53837401ab6b128b20e8a989";
      sha256 = "1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
    }) {}).ghcide-ghc865;

  cabal-plan =
    super.haskell.lib.justStaticExecutables
      (super.haskell.lib.overrideCabal super.haskellPackages.cabal-plan (old: {
        configureFlags =
          (old.configureFlags or []) ++ [ "-flicense-report" ];
        executableHaskellDepends =
          (old.executableHaskellDepends or []) ++ (with self.haskellPackages; [ tar zlib ]);
      }));

  lf =
    super.buildGoModule {
      pname = "lf";
      version = "r14";
      src = super.fetchFromGitHub {
        owner = "gokcehan";
        repo = "lf";
        rev = "r14";
        sha256 = "0kl9yrgph1i0jbxhlg3k0411436w80xw1s8dzd7v7h2raygkb4is";
      };
      vendorSha256 = "1zb2z3c2w4gnq9cjczg1y7r7jg4mlrm2hsb12dqd9w8mh44rvr37";
    };

  gcoreutils =
    super.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };
}

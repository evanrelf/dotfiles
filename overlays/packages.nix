self: super:

{
  cabal-plan =
    super.haskell.lib.justStaticExecutables
      (super.haskell.lib.overrideCabal super.haskellPackages.cabal-plan (old: {
        configureFlags =
          (old.configureFlags or []) ++ [ "-flicense-report" ];
        executableHaskellDepends =
          (old.executableHaskellDepends or []) ++ (with self.haskellPackages; [ tar zlib ]);
      }));

  coin =
    let
      coinSound = super.fetchurl {
        url = "https://themushroomkingdom.net/sounds/wav/smw/smw_coin.wav";
        sha256 = "18c7dfhkaz9ybp3m52n1is9nmmkq18b1i82g6vgzy7cbr2y07h93";
      };
    in
      super.writeShellScriptBin "coin" ''
         ${self.sox}/bin/play --no-show-progress ${coinSound}
      '';

  comma =
    import (super.fetchFromGitHub {
      owner = "shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
    }) { pkgs = self; };

  gay =
    super.python3Packages.buildPythonPackage rec {
      pname = "gay";
      version = "1.2.1";
      src = super.python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "0kjraippbkm1fgmky7dfww4an2yj53y4fcsganps1dhgn1mfs2ks";
      };
    };

  gcoreutils =
    super.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };

  ghcide =
    (import (super.fetchFromGitHub {
      owner = "cachix";
      repo = "ghcide-nix";
      rev = "67493b873e1a5e5d53837401ab6b128b20e8a989";
      sha256 = "1zq5g7ka99vcyqbg5l1bx0rliq3ihig37nzczk0wdwidjyxjghf9";
    }) {}).ghcide-ghc865;

  # gormolu =
  #   let
  #     source =
  #       super.fetchFromGitHub {
  #         owner = "tweag";
  #         repo = "ormolu";
  #         rev = "b1fc69265f8688490b16de83bb235d29aa25bcb0";
  #         sha256 = "1yk3yh0rg0gcnhyd52jwqgah09yczbk21gb80xhrighacmvdsnwn";
  #       };
  #     haskellPackage =
  #       (import source { pkgs = self; }).ormolu.override {
  #         ghc-lib-parser = self.haskellPackages.ghc-lib-parser_8_10_1_20200523;
  #       };
  #     executable =
  #       super.haskell.lib.justStaticExecutables haskellPackage;
  #   in
  #     super.runCommandLocal "gormolu" {} ''
  #       mkdir -p $out/bin
  #       ln -s ${executable}/bin/ormolu $out/bin/gormolu
  #     '';

  home-manager =
    super.home-manager.overrideAttrs (old: {
      version = "HEAD";
      src = super.fetchFromGitHub {
        owner = "rycee";
        repo = "home-manager";
        rev = "7f7348b47049e8d25fb5b98db1d6215f8f643f0d";
        sha256 = "1s8j8h42k354xzwlqizbzxlqvsagx7yrxl20qfxgxmx522ajw63p";
      };
    });

  kakoune =
    super.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = super.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "d79b0053239047525c1a8d659a7179c09dee47b8";
        sha256 = "1c4xfavlax5mhjlvqrg2zkqs1zqh9kdxrp661giwvlll2sizbk7x";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

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

  lorri =
    import (super.fetchFromGitHub {
      owner = "target";
      repo = "lorri";
      rev = "1.1";
      sha256 = "0wbkx8hmikngfp6fp0y65yla22f3k0jszq8a6pas80q0b33llwm5";
    }) { pkgs = self; };

  nix-tree =
    (import (super.fetchFromGitHub {
      owner = "utdemir";
      repo = "nix-tree";
      rev = "c3c77ede124e91c3275a994f9e57cab360e6a13a";
      sha256 = "0nyln9cxybqdcp9pdh5qjhhmbl0bn8f9ghh4cn0q2wmsldqpl0ai";
    })).nix-tree;

  ormolu =
    super.haskell.lib.justStaticExecutables
      (super.haskellPackages.ormolu_0_1_0_0.override {
        ghc-lib-parser = self.haskellPackages.ghc-lib-parser_8_10_1_20200523;
      });
}

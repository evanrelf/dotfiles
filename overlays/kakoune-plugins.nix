final: prev:

let
  sources = {
    "better-haskell-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "better-haskell.kak";
      rev = "399c844b6627f61a1c884825c8b616493a80766a";
      hash = "sha256-OmuaW2L34sDE5C2mXUH5JEtt3qh85PCk8hZqoIzvwZc=";
    };

    "byline-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "a27d109b776c60e11752eeb3207c989a5e157fc0";
      hash = "sha256-Aa0UnioD20HfGiTtC7Tmbs+xYgaytz3pUsXQWkzrLYg=";
    };

    "grep-write-kak" = final.fetchFromGitHub {
      owner = "jtrv";
      repo = "grep-write.kak";
      rev = "183afe2ece341f6366ffdb9039dbd061e305ceb8";
      hash = "sha256-qPetxg69AYHSeINW2tKQ+BL4ShA+s/mWOmhCgVgoWSU=";
    };

    "kak-fetch" = final.fetchFromGitHub {
      owner = "mmlb";
      repo = "kak-fetch";
      rev = "bf4a28bb9bb68fd2b568996dc5cb939bcabfe36a";
      hash = "sha256-/rKtuDn3s+rjX+G5osZXjCgJMhdfI5DvtW/SBRkBeVY=";
    };

    "number-toggle-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "number-toggle.kak";
      rev = "d6b67642d0950c4a8089233f4260b50d80ba0c48";
      hash = "sha256-C5Dg2/kCtNSztwjClnZkJHy89h5WbvlstbU2j//6lYA=";
    };

    "primer-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "primer.kak";
      rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
      hash = "sha256-eGxzfbcg31bwoDq+EdAp5peoXTz8e1j+j4neCfuyxxw=";
    };
  };

in
{
  kakoune = final.wrapKakoune final.kakoune-unwrapped {
    plugins =
      builtins.attrValues
        (builtins.mapAttrs
          (name: src: prev.kakouneUtils.buildKakounePluginFrom2Nix {
            inherit name src;
          })
          sources);
  };
}

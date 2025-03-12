final: prev:

let
  sources = {
    "better-haskell-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "better-haskell.kak";
      rev = "069690575aa92e34338d8102594f797c1e1fa41a";
      hash = "sha256-X3v/8zNM8zauWSryGs4xh7nI2wBNaVqSkLCVItRDnPA=";
    };

    "byline-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "a27d109b776c60e11752eeb3207c989a5e157fc0";
      hash = "sha256-Aa0UnioD20HfGiTtC7Tmbs+xYgaytz3pUsXQWkzrLYg=";
    };

    # TODO: Audit this, maybe make my own simpler version that also handles 2D
    # parenthesized coordinates.
    "kak-fetch" = final.fetchFromGitHub {
      owner = "mmlb";
      repo = "kak-fetch";
      rev = "bf4a28bb9bb68fd2b568996dc5cb939bcabfe36a";
      hash = "sha256-/rKtuDn3s+rjX+G5osZXjCgJMhdfI5DvtW/SBRkBeVY=";
    };

    "primer-kak" = final.symlinkJoin {
      name = "primer-kak";
      paths = [
        (final.fetchFromGitHub {
          owner = "evanrelf";
          repo = "primer.kak";
          rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
          hash = "sha256-eGxzfbcg31bwoDq+EdAp5peoXTz8e1j+j4neCfuyxxw=";
        })
        (final.writeTextDir "primer-kak-column-color.kak" ''
          declare-option str column_color "rgb:f6f8fa"
        '')
      ];
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

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

    "locus-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "locus.kak";
      rev = "e5053300401325ed2cc5c8421fe57e3519d2762b";
      hash = "sha256-wqlAPhwp7SxOIIv57dYP7ZhOCpPg5CL9yx9gaeHE71g=";
    };

    "primer-kak" = final.symlinkJoin {
      name = "primer-kak";
      paths = [
        (final.fetchFromGitHub {
          owner = "evanrelf";
          repo = "primer.kak";
          rev = "4c64b21fb12d14a0c5d687717f5e767a80cdf7c0";
          hash = "sha256-nUg/0MBfbcJy6nlSx5gswtsq5F5a9BYOYmprI4SEDYw=";
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

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
      rev = "dd3f056587afddf23fafd76d653b863efa9a9cb3";
      hash = "sha256-O0i1p+D6NUcPxEs4gpqRy/63h/fgRs77zzObGESqfLo=";
    };

    "open-github-kak" = final.fetchFromGitHub {
      owner = "evanrelf";
      repo = "open-github.kak";
      rev = "dfdd947f5aaa9dc2794011d712cce54d4d9b9213";
      hash = "sha256-0FkFT+akDNovFcY8LTC8ANWZjzJjk2q90MXWUjPiWV4=";
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

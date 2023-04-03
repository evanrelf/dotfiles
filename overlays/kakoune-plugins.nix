pkgsFinal: pkgsPrev:

let
  sources = {
    "better-haskell-kak" = {
      owner = "evanrelf";
      repo = "better-haskell.kak";
      rev = "399c844b6627f61a1c884825c8b616493a80766a";
      sha256 = "sha256-OmuaW2L34sDE5C2mXUH5JEtt3qh85PCk8hZqoIzvwZc=";
    };

    "byline-kak" = {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "a27d109b776c60e11752eeb3207c989a5e157fc0";
      sha256 = "sha256-Aa0UnioD20HfGiTtC7Tmbs+xYgaytz3pUsXQWkzrLYg=";
    };

    "number-toggle-kak" = {
      owner = "evanrelf";
      repo = "number-toggle.kak";
      rev = "d6b67642d0950c4a8089233f4260b50d80ba0c48";
      sha256 = "sha256-C5Dg2/kCtNSztwjClnZkJHy89h5WbvlstbU2j//6lYA=";
    };

    "primer-kak" = {
      owner = "evanrelf";
      repo = "primer.kak";
      rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
      sha256 = "0767nbxhkpl9izz5hyzw7ifsi5z657813gisl3q5dpr0nxyp6v3q";
    };
  };

in
{
  kakoune = pkgsPrev.wrapKakoune pkgsFinal.kakoune-unwrapped {
    plugins =
      builtins.attrValues
        (builtins.mapAttrs
          (name: github: pkgsPrev.kakouneUtils.buildKakounePluginFrom2Nix {
            inherit name;
            src = pkgsPrev.fetchFromGitHub github;
          })
          sources);
  };
}

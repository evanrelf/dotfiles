final: prev:

let
  sources = {
    "better-haskell-kak" = {
      owner = "evanrelf";
      repo = "better-haskell.kak";
      rev = "399c844b6627f61a1c884825c8b616493a80766a";
      hash = "sha256-OmuaW2L34sDE5C2mXUH5JEtt3qh85PCk8hZqoIzvwZc=";
    };

    "byline-kak" = {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "a27d109b776c60e11752eeb3207c989a5e157fc0";
      hash = "sha256-Aa0UnioD20HfGiTtC7Tmbs+xYgaytz3pUsXQWkzrLYg=";
    };

    "git-revise-kak" = {
      owner = "evanrelf";
      repo = "git-revise.kak";
      rev = "ec9a7995a9eefc44f682e4c198a27dbc0921b283";
      hash = "sha256-6GoiJsNqxe0dzxzpnjw50dSE94o2AyCX59XAiWfSSGA=";
    };

    "kak-fetch" = {
      owner = "mmlb";
      repo = "kak-fetch";
      rev = "bf4a28bb9bb68fd2b568996dc5cb939bcabfe36a";
      hash = "sha256-/rKtuDn3s+rjX+G5osZXjCgJMhdfI5DvtW/SBRkBeVY=";
    };

    "number-toggle-kak" = {
      owner = "evanrelf";
      repo = "number-toggle.kak";
      rev = "d6b67642d0950c4a8089233f4260b50d80ba0c48";
      hash = "sha256-C5Dg2/kCtNSztwjClnZkJHy89h5WbvlstbU2j//6lYA=";
    };

    "parinfer-rust" = {
      owner = "eraserhd";
      repo = "parinfer-rust";
      rev = "3c769707ad3b18d530f696fe42a9d495139149ab";
      hash = "sha256-EVFuCZo6lYXEOWQzTW7BEfB5PIbO4YA4dCMo7N4oWrM=";
    };

    "primer-kak" = {
      owner = "evanrelf";
      repo = "primer.kak";
      rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
      hash = "sha256-eGxzfbcg31bwoDq+EdAp5peoXTz8e1j+j4neCfuyxxw=";
    };
  };

in
{
  kakoune = prev.wrapKakoune final.kakoune-unwrapped {
    plugins =
      builtins.attrValues
        (builtins.mapAttrs
          (name: github: prev.kakouneUtils.buildKakounePluginFrom2Nix {
            inherit name;
            src = prev.fetchFromGitHub github;
          })
          sources);
  };
}

pkgsFinal: pkgsPrev:

let
  sources = {
    "primer-kak" = {
      owner = "evanrelf";
      repo = "primer.kak";
      rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
      sha256 = "0767nbxhkpl9izz5hyzw7ifsi5z657813gisl3q5dpr0nxyp6v3q";
    };

    "byline-kak" = {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "3636f8f83624cf9d44c0e1242e5af2319abbca5f";
      sha256 = "0yxialy0n35hw2wcnpacax1b19jqwbm7i1kq66x7g4msadlqqigj";
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

pkgsFinal: pkgsPrev:

let
  sources = {
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

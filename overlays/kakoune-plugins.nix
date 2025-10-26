final: prev:

let
  sources = {
    better-haskell-kak =
      final.inputs.better-haskell-kak.outPath;

    byline-kak =
      final.inputs.byline-kak.outPath;

    locus-kak =
      final.inputs.locus-kak.outPath;

    open-github-kak =
      final.inputs.open-github-kak.outPath;
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

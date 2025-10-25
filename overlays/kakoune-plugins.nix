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

    primer-kak =
      final.symlinkJoin {
        name = "primer-kak";
        paths = [
          final.inputs.primer-kak.outPath
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

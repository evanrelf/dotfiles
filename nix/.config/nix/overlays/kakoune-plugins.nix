pkgsFinal: pkgsPrev:

let
  sources = {
    "primer-kak" = {
      owner = "evanrelf";
      repo = "primer.kak";
      rev = "4c0c1c48de189e1f1a8582c57d89f8008b358430";
      sha256 = "0767nbxhkpl9izz5hyzw7ifsi5z657813gisl3q5dpr0nxyp6v3q";
    };

    "number-toggle-kak" = {
      owner = "evanrelf";
      repo = "number-toggle.kak";
      rev = "9e7642a6fb3656740d782c5d88a8aeb03dbdc065";
      sha256 = "0q8874j128dl9xq4yfcgk4ny26ybn8ldbbl7shsszs658582dlly";
    };

    "byline-kak" = {
      owner = "evanrelf";
      repo = "byline.kak";
      rev = "3636f8f83624cf9d44c0e1242e5af2319abbca5f";
      sha256 = "0yxialy0n35hw2wcnpacax1b19jqwbm7i1kq66x7g4msadlqqigj";
    };

    "reselect-kak" = {
      owner = "evanrelf";
      repo = "reselect.kak";
      rev = "f004700e851c1c25fe950e63c673135ae1396a5f";
      sha256 = "1gyc7q0scibmgjg2vddwjd1pxpyc1hif459qb0p7g7qdwqdbmbar";
    };

    "replace-mode-kak" = {
      owner = "alexherbo2";
      repo = "replace-mode.kak";
      rev = "5f4c73cdbaf5aeb964ee35ad4b9081b233af90c0";
      sha256 = "1cmylx99bm7jwfb4hclb69sdc4n8f29ssyy2byjiw53ni9rnc8q0";
    };

    "kakoune-surround" = {
      owner = "h-youhei";
      repo = "kakoune-surround";
      rev = "efe74c6f434d1e30eff70d4b0d737f55bf6c5022";
      sha256 = "09fd7qhlsazf4bcl3z7xh9z0fklw69c5j32hminphihq74qrry6h";
    };
  };

  sourceToPlugin = name: github:
    pkgsPrev.kakouneUtils.buildKakounePluginFrom2Nix {
      inherit name;
      src =
        pkgsPrev.fetchFromGitHub
          (builtins.removeAttrs
            github
            [ "disabled" ]);
    };

in
{
  kakoune = pkgsPrev.wrapKakoune pkgsFinal.kakoune-unwrapped {
    plugins =
      builtins.attrValues
        (builtins.mapAttrs
          sourceToPlugin
          (pkgsPrev.lib.filterAttrs (_: x: !(x.disabled or false)) sources));
  };
}

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

    "parinfer-rust" = {
      owner = "eraserhd";
      repo = "parinfer-rust";
      rev = "afcacc8ef1fbf23d804d2c4eb01a419c50565e44";
      sha256 = "1xwahgwjv1ylmy0bwbsisycjlz5r9i1gxz20392a8f8019zhjx90";
    };

    "kakoune-fandt" = {
      disabled = true;
      owner = "listentolist";
      repo = "kakoune-fandt";
      rev = "6b035782c2437708917ff1e4d3c05e33678e42dc";
      sha256 = "0vmvlh1cw89dhd1v50bychszyk5ymh2mk891cg10nf47yb0paiv6";
    };

    "auto-pairs-kak" = {
      disabled = true;
      owner = "alexherbo2";
      repo = "auto-pairs.kak";
      rev = "fd735ec149ef0d9ca5f628a95b1e52858b5afbdc";
      sha256 = "07795kv9njlnp6mckwv141ny2ns6wyf5r0dfjaxh9ngd105zgif1";
    };

    # Dependency of `auto-pairs.kak`
    "prelude-kak" = {
      disabled = true;
      owner = "kakounedotcom";
      repo = "prelude.kak";
      rev = "5dbdc020c546032885c1fdb463e366cc89fc15ad";
      sha256 = "1pncr8azqvl2z9yvzhc68p1s9fld8cvak8yz88zgrp5ypx2cxl8c";
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

pkgsFinal: pkgsPrev:

# TODO: Specify plugins' configurations in Nix!

let
  plugins = {
    "primer-kak" = { repo = "primer.kak"; };
    "number-toggle-kak" = { repo = "number-toggle.kak"; };
    "byline-kak" = { repo = "byline.kak"; };
    "reselect-kak" = { repo = "reselect.kak"; };

    "replace-mode-kak" = {
      owner = "alexherbo2";
      repo = "replace-mode.kak";
      rev = "5f4c73cdbaf5aeb964ee35ad4b9081b233af90c0";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };

    "kakoune-fandt" = {
      owner = "listentolist";
      repo = "kakoune-fandt";
      rev = "6b035782c2437708917ff1e4d3c05e33678e42dc";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };

    "kakoune-surround" = {
      owner = "h-youhei";
      repo = "kakoune-surround";
      rev = "efe74c6f434d1e30eff70d4b0d737f55bf6c5022";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };

    "auto-pairs-kak" = {
      owner = "alexherbo2";
      repo = "auto-pairs.kak";
      rev = "fd735ec149ef0d9ca5f628a95b1e52858b5afbdc";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };

    # Dependency of `auto-pairs.kak`
    "prelude-kak" = {
      owner = "kakounedotcom";
      repo = "prelude.kak";
      rev = "5dbdc020c546032885c1fdb463e366cc89fc15ad";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };
  };

  # Plugins set to `null` will use the existing source from
  # `pkgsFinal.kakounePlugins`
  sources = pkgsPrev.filterAttrs (_: value: value != null) plugins;

  sourceToPlugin = name: github: {
    inherit name;
    value = pkgsPrev.kakouneUtils.buildKakounePlugin {
      inherit name;
      src = pkgsPrev.fetchFromGitHub ({
        owner = "evanrelf";
        rev = "main";
      } // github);
    };
  };

in {
  kakoune = pkgsPrev.wrapKakoune pkgsFinal.kakoune-unwrapped {
    plugins =
      builtins.map
        (name: pkgsFinal.kakounePlugins."${name}")
        (builtins.attrNames plugins);
  };

  kakounePlugins = pkgsPrev.kakounePlugins.override (old: {
    overrides =
      pkgsPrev.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (_: _: builtins.mapAttrs sourceToPlugin sources);
  });
}

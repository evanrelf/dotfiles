pkgsFinal: pkgsPrev:

let
  gprefix = drv:
    pkgsPrev.runCommandLocal "gprefix-${drv.name}" { } ''
      mkdir -p "$out/bin"
      for bin in ${drv}/bin/*; do
        ln -s "$bin" "$out/bin/g$(basename $bin)"
      done
      ln -s ${drv}/share "$out/share"
    '';

in
{
  comma =
    # TODO: This is a hack
    if pkgsPrev.system == "aarch64-darwin" then
      pkgsPrev.inputs.comma.defaultPackage."x86_64-darwin"
    else
      pkgsPrev.inputs.comma.defaultPackage."${pkgsPrev.system}";

  coreutils-gprefix =
    (pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    }).overrideAttrs (prev: {
      doCheck = false;
    });

  dotfiles =
    # TODO:
    #
    # Silence this error message when `build`ing or `switch`ing:
    #
    #```
    # No configuration file found. Please create one at /Users/evanrelf/.config/nixpkgs/home.nix
    #```
    #
    # One of these should work:
    #
    # ```
    # export HOME_MANAGER_CONFIG="/dev/null"
    # export HOME_MANAGER_CONFIG="${pkgs.writeText "home.nix" "{ }"}"
    # ```
    #
    # ...but it produces this error message:
    #
    # ```
    # error: file 'home-manager/home-manager/home-manager.nix' was not found in the Nix search path (add it using $NIX_PATH or -I)
    # ```
    pkgsPrev.writeShellScriptBin "dotfiles" ''
      set -euo pipefail
      IFS=$'\n\t'
      export PATH="${pkgsFinal.home-manager}/bin:$PATH"
      hostname=$(hostname -s)
      trace() { set -x; $@; { set +x; } 2>/dev/null; }
      if [ "$(nix-instantiate --eval --expr 'builtins ? getFlake')" = "true" ]; then
        trace home-manager --flake .#$hostname $@
      else
        if [ "$#" = "1" ] && [ "$1" = "switch" ]; then
          echo "Falling back to non-flake switch"
          temp=$(mktemp -d)
          trap "rm -rf $temp" EXIT
          trace nix build --file . homeConfigurations.$hostname -o $temp/result
          config=$(readlink "$temp/result")
          trace $config/activate
        elif [ "$#" = "1" ] && [ "$1" = "build" ]; then
          echo "Falling back to non-flake build"
          trace nix build --file . homeConfigurations.$hostname
        else
          echo "Unsupported arguments in fallback mode"
          trace home-manager $@
        fi
      fi
    '';

  emacsCustom =
    (pkgsPrev.emacsPackagesGen pkgsFinal.emacsGcc).emacsWithPackages (p: [ p.vterm ]);

  findutils-gprefix = gprefix pkgsFinal.findutils;

  firefox = pkgsPrev.wrapFirefox pkgsPrev.firefox-unwrapped {
    extraPolicies = {
      # Personal preferences
      SearchEngines.Default = "DuckDuckGo";
      ShowHomeButton = false;

      # Prevent tracking
      DisableFirefoxStudies = true;
      DisableTelemetry = true;
      EnableTrackingProtection = {
        Value = true;
        Cryptomining = true;
        Fingerprinting = true;
      };

      # Disable annoyances
      DontCheckDefaultBrowser = true;
      UserMessaging = {
        ExtensionRecommendations = false;
        SkipOnboarding = true;
      };
      NewTabPage = false;
      NoDefaultBookmarks = true;

      # Disable unused features
      DisablePocket = true;
      OfferToSaveLogins = false;
      PasswordManagerEnabled = false;
      DisableMasterPasswordCreation = true;
      FirefoxHome = {
        Pocket = false;
        Snippets = false;
      };
    };
    extraPrefs = ''
      // Personal preferences
      lockPref("browser.ctrlTab.recentlyUsedOrder", false)
      // lockPref("browser.newtabpage.enabled", false);

      // Scrolling
      lockPref("general.smoothScroll", false);
      lockPref("apz.gtk.kinetic_scroll.enabled", false);
      lockPref("mousewheel.default.delta_multiplier_y", 50);

      // Disable nags
      lockPref("browser.aboutConfig.showWarning", false);
      lockPref("browser.shell.checkDefaultBrowser", false);
    '';
  };

  gitAndTools = pkgsPrev.gitAndTools // {
    gitFull = pkgsPrev.gitAndTools.gitFull.overrideAttrs (prev: {
      doInstallCheck = false;
    });
  };

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  home-manager = pkgsPrev.inputs.home-manager.defaultPackage."${pkgsFinal.system}";

  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (prev: rec {
      version = "2021.11.08";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "v${version}";
        sha256 = "02ads7d2scvab9pmsj77hc2kh9fpshmmblmwzw6w26zm86vqrhcl";
      };
      preConfigure = ''
        ${prev.preConfigure}
        export version="${version}"
      '';
    });

  nerdfonts = pkgsPrev.nerdfonts.override { fonts = [ "Iosevka" ]; };

  patat =
    let
      source = pkgsPrev.fetchFromGitHub {
        owner = "evanrelf";
        repo = "patat";
        rev = "b5f525bd2f3971fba231f378fa1b8f02c63860ba";
        sha256 = "sha256-4m9iYRlzDzd4d1a+Qo/n64ER0+elAoW838CF/qgDjpE=";
      };
    in
    pkgsPrev.haskell.lib.doJailbreak
      (pkgsPrev.haskellPackages.callCabal2nix "patat" source { });
}

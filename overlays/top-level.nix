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

  home-rebuild =
    pkgsPrev.writeShellScriptBin "home-rebuild" ''
      set -Eeuo pipefail
      IFS=$'\n\t'
      if [ -n "''${DOTFILES:-}" ]; then
        cd "$DOTFILES"
      fi
      export PATH="${pkgsFinal.home-manager}/bin:$PATH"
      export PATH="${pkgsFinal.jq}/bin:$PATH"
      hostname=$(hostname -s)
      if [ "$(nix-instantiate --eval --expr 'builtins ? getFlake')" = "true" ]; then
        home-manager --flake .#"$hostname" "$@"
      else
        if [ "$#" = "1" ] && [ "$1" = "switch" ]; then
          echo "Falling back to non-flake switch"
          temp=$(mktemp -d)
          trap 'rm -rf $temp' EXIT
          nix build --file . homeConfigurations."$hostname" -o "$temp/result"
          "$(readlink "$temp/result")"/activate
        elif [ "$#" = "1" ] && [ "$1" = "build" ]; then
          echo "Falling back to non-flake build"
          nix build --file . homeConfigurations."$hostname"
        else
          echo "Unsupported arguments in fallback mode"
          home-manager "$@"
        fi
      fi
    '';

  emacsCustom =
    pkgsPrev.emacsWithPackagesFromUsePackage {
      package = pkgsFinal.emacsGcc;
      config = ../configs/emacs/.config/emacs/init.el;
      extraEmacsPackages = p: with p; [
        apheleia
        libgit
        vterm
      ];
      alwaysEnsure = true;
    };

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

  home-manager =
    pkgsPrev.inputs.home-manager.defaultPackage."${pkgsFinal.system}";

  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  jujutsu =
    (pkgsPrev.makeRustPlatform {
      inherit (pkgsFinal.fenix.minimal) cargo rustc;
    }).buildRustPackage {
      name = "jujutsu";
      src = pkgsPrev.fetchFromGitHub {
        owner = "martinvonz";
        repo = "jj";
        rev = "ba01c512aed9fee71ae1cd83709bcd4181d25876";
        sha256 = "sha256-OhbKQh8mrEazpml4QCs4H6+5HBVv6dQEACUSybO0J34=";
      };
      cargoSha256 = "sha256-EWzAXSAPE+hE7g8aDdUoKEKWZztIz53ajPxi6j49lCQ=";
      buildInputs = pkgsPrev.lib.optionals pkgsPrev.stdenv.isDarwin [
        pkgsFinal.darwin.apple_sdk.frameworks.Security
        pkgsFinal.darwin.apple_sdk.frameworks.SystemConfiguration
      ];
      NIX_LDFLAGS =
        pkgsPrev.lib.optionalString
          pkgsPrev.stdenv.isDarwin
          "-framework Security -framework SystemConfiguration";
      OPENSSL_NO_VENDOR = true;
      OPENSSL_LIB_DIR = "${pkgsFinal.openssl.out}/lib";
      OPENSSL_INCLUDE_DIR = "${pkgsFinal.openssl.dev}/include";
    };

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

  zig =
    pkgsPrev.inputs.zig-overlay.packages."${pkgsPrev.system}"."0.9.0";
}

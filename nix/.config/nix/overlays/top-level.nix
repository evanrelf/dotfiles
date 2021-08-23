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
  coreutils-gprefix =
    pkgsPrev.coreutils.override {
      singleBinary = false;
      withPrefix = true;
    };

  emacsCustom =
    let
      emacsGccDarwin = (import (pkgsPrev.fetchFromGitHub {
        owner = "siraben";
        repo = "nix-gccemacs-darwin";
        rev = "c3800c44331dca424fa525d8bb08e49ee37fdfb3";
        sha256 = "1m8qqhfij37q7sjqa91cmymr6az2vva15jghzywhpq3j73ipmqy1";
      })).emacsGccDarwin;

      emacs =
        builtins.getAttr builtins.currentSystem {
          "x86_64-linux" = pkgsFinal.emacsGcc;
          "x86_64-darwin" = emacsGccDarwin;
        };
    in
    (pkgsPrev.emacsPackagesGen emacs).emacsWithPackages (p: [ p.vterm ]);

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
    gitFull = pkgsPrev.gitAndTools.gitFull.overrideAttrs (old: {
      doInstallCheck = false;
    });
  };

  gitignoreSource =
    let
      source = pkgsPrev.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore.nix";
        rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
        sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
      };
    in
    (import source { inherit (pkgsFinal) lib; }).gitignoreSource;

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "e454b393cfaa69d270dda842b40d9c8fc3d672e6";
        sha256 = "1cvbqyzgyjjlc2qqs39hrppxpbxmn37c51qi9746lxmpd82ri4z7";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  nerdfonts = pkgsPrev.nerdfonts.override { fonts = [ "Iosevka" ]; };

  ormoloog =
    let
      ormolu =
        pkgsPrev.ormolu.overrideAttrs (old: {
          src = pkgsPrev.fetchFromGitHub {
            owner = "google";
            repo = "ormolu";
            rev = "ffdf145bbdf917d54a3ef4951fc2655e35847ff0";
            sha256 = "0m4azjy90knahcg6kpa8sxvkwv8vf8dlip2bcmz6p0x934183bxb";
          };
        });
    in
    pkgsPrev.runCommandLocal "ormoloog" { } ''
      mkdir -p \
        $out/bin/ \
        $out/share/bash-completion/completions/ \
        $out/share/fish/vendor_completions.d/ \
        $out/share/zsh/vendor-completions/
      ln -s ${ormolu.bin}/bin/ormolu $out/bin/ormoloog
      ln -s \
        ${ormolu.bin}/share/bash-completion/completions/ormolu \
        $out/share/bash-completion/completions/ormoloog
      ln -s \
        ${ormolu.bin}/share/fish/vendor_completions.d/ormolu.fish \
        $out/share/fish/vendor_completions.d/ormoloog.fish
      ln -s \
        ${ormolu.bin}/share/zsh/vendor-completions/_ormolu \
        $out/share/zsh/vendor-completions/_ormoloog
    '';

  patat =
    let
      source = pkgsPrev.fetchFromGitHub {
        owner = "evanrelf";
        repo = "patat";
        rev = "4d976d463aedc1027a31ea70ae2d12d64c915cc6";
        sha256 = "1760bddpz0p8n88n1m03wz8yzzs7x1sgd2rf26fs4x6way3jym6b";
      };
    in
    pkgsPrev.haskell.lib.doJailbreak
      (pkgsPrev.haskellPackages.callCabal2nix "patat" source { });

  tmux-thumbs = pkgsPrev.rustPlatform.buildRustPackage rec {
    pname = "tmux-thumbs";
    version = "0.5.1";
    src = pkgsPrev.fetchFromGitHub {
      owner = "fcsonline";
      repo = pname;
      rev = version;
      sha256 = "0lack8x54ic52sj8wjg7ybwirs9m5zkxlqg7y22qmih9nsraskxd";
    };
    cargoSha256 = "1qyfp7kswd3ppbj4jwrkp2b2lhs5pi8f6mxrjffjxa2bsiz6spzw";
  };

  zig = pkgsPrev.stdenv.mkDerivation rec {
    pname = "zig";
    version = "0.9.0-dev.730+011a46838";
    src =
      let
        system =
          if builtins.currentSystem == "x86_64-darwin" then {
            name = "macos-x86_64";
            sha256 = "03bpvy29cy6vyh570khimqbxsjnrybn0y7lgfybh8dhah7w2vn1w";
          } else {
            name = "linux-x86_64";
            sha256 = "09amvngffpz86d368jj8fwcpn6vlzqfln3mpy4xlc8i0qv089avi";
          };
      in
      builtins.fetchTarball {
        url = "https://ziglang.org/builds/zig-${system.name}-${version}.tar.xz";
        inherit (system) sha256;
      };
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      mv zig $out/bin/zig
      mv lib $out/lib
    '';
  };
}

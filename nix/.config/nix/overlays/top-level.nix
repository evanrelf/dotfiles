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
        owner = "twlz0ne";
        repo = "nix-gccemacs-darwin";
        rev = "088f97e2939f33d6983fb90649a9c51d572736ec";
        sha256 = "0mdbgl82r0dpgsz71i2npfy6kyd16637sk7glagwwdz7l0zxxmwn";
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

  gnugrep-gprefix = gprefix pkgsFinal.gnugrep;

  iosevka-bin = pkgsPrev.iosevka-bin.override { variant = "ss08"; };

  kakoune-unwrapped =
    pkgsPrev.kakoune-unwrapped.overrideAttrs (old: rec {
      version = "HEAD";
      src = pkgsPrev.fetchFromGitHub {
        owner = "mawww";
        repo = "kakoune";
        rev = "545db22ae464668d36fad500690a77c019936910";
        sha256 = "08v31b70v815sjfzzz9jl3flk1k0ynv0n4dv68x0rkkkmxvzmnls";
      };
      preConfigure = ''
        ${old.preConfigure}
        export version="${version}"
      '';
    });

  neovim-unwrapped =
    pkgsPrev.neovim-unwrapped.overrideAttrs (old: {
      version = "0.5.0-dev+1157-g0ab88c2ea";
      src =
        pkgsPrev.fetchFromGitHub {
          owner = "neovim";
          repo = "neovim";
          rev = "0ab88c2ea80caa7cda97b3a8479d0d32e4636ab6";
          sha256 = "07jiv9c3032lrhmd3dvqv2v5l35bdn39jqi48qsjj220slrsrl53";
        };
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgsFinal.tree-sitter ];
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
}

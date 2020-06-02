let
  lib = import ./lib.nix;

  inherit (import ./nixpkgs.nix) legacy stable unstable sources;

  custom =
    { kakoune =
        unstable.kakoune-unwrapped.overrideAttrs (old: rec {
          version = "HEAD";
          src = lib.fetchGitHub {
            owner = "mawww";
            repo = "kakoune";
            rev = "c585107ab5e7155f7da648c3752cf360f7156177";
            sha256 = "1rjnhkzwrwxkbi78rpbl06d815jdkpkfpfcv5ppclvpwyqfd98zc";
          };
          preConfigure = ''
            ${old.preConfigure}
            export version="${version}"
          '';
        });
      lorri =
        import (lib.fetchGitHub {
          owner = "target";
          repo = "lorri";
          rev = "896391ed257e6f3cd5bf7a2e802d2761c3be1ff5";
          sha256 = "143ds2cdvxf1sj8g4aw6jaglg719sqb278j6kfclb7q0ykdhirr3";
        }) {};
      comma =
        import (lib.fetchGitHub {
          owner = "shopify";
          repo = "comma";
          rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
          sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
        }) { pkgs = unstable; };
      ormolu =
        unstable.haskell.lib.justStaticExecutables
          (unstable.haskellPackages.ormolu_0_0_5_0.override {
            ghc-lib-parser = unstable.haskellPackages.ghc-lib-parser_8_10_1_20200412;
          });
      ghcide =
        (import (lib.fetchGitHub {
          owner = "cachix";
          repo = "ghcide-nix";
          rev = "f940ec611cc6914693874ee5e024eba921cab19e";
          sha256 = "0vri0rivdzjvxrh6lzlwwkh8kzxsn82jp1c2w5rqzhp87y6g2k8z";
        }) {}).ghcide-ghc865;
      cabal-plan =
        unstable.haskell.lib.justStaticExecutables
          (unstable.haskell.lib.overrideCabal unstable.haskellPackages.cabal-plan (old: {
            configureFlags =
              (old.configureFlags or []) ++ [ "-flicense-report" ];
            executableHaskellDepends =
              (old.executableHaskellDepends or []) ++ (with unstable.haskellPackages; [ tar zlib ]);
          }));
      iosevka =
        # To install on macOS:
        # 1. cd $(nix-env --query packages --out-path | awk '{print $2}')"/share/fonts/iosevka-pro/"
        # 2. open .
        # 3. Select all
        # 4. Open
        unstable.iosevka.override {
          set = "pro";
          privateBuildPlan = {
            family = "Iosevka Pro";
            design = [
              # PragmataPro style
              "ss08"
              # Make "Term" variant
              "sp-term"
              # Add Haskell ligatures
              "ligset-haskell"
              # Add != and !== ligatures
              "calt-exeq"
              # Add <!-- and <!--- ligatures
              "calt-html-comment"
            ];
          };
        };
      gcoreutils = unstable.coreutils.override {
        singleBinary = false;
        withPrefix = true;
      };
    };

  packages = {
    universal = (with stable; [
      nix-linter
    ]) ++ (with unstable; [
      # (haskell.lib.justStaticExecutables haskellPackages.ghcide)
      (aspellWithDicts (d: with d; [ en en-computers en-science ]))
      (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
      (haskell.lib.justStaticExecutables haskellPackages.nix-derivation)
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      borgbackup
      cabal-install
      cabal2nix
      cachix
      dhall
      dhall-json
      direnv
      exa
      fd
      fish
      fzf
      ghcid
      git
      git-revise
      gitAndTools.delta
      hlint
      htop
      httpie
      jq
      neovim
      nix-diff
      nix-prefetch-git
      nixpkgs-fmt
      nodejs
      pandoc
      pijul
      python3
      ripgrep
      rsync
      rustup
      sd
      shellcheck
      stack
      tealdeer
      tectonic
      tmux
      tokei
      universal-ctags
      youtube-dl
    ]) ++ (with custom; [
      # iosevka
      cabal-plan
      comma
      ghcide
      kakoune
      lorri
      ormolu
    ]);
    linux = (with unstable; [
      acpi
      chromium
      dmenu
      emacs
      feh
      firefox-wayland
      gnome3.cheese
      gnome3.eog
      gnome3.evince
      gnome3.gnome-boxes
      gnome3.gnome-disk-utility
      gnome3.gnome-system-monitor
      gnome3.nautilus
      gnupg
      grim
      kitty
      mako
      mpv
      slurp
      spotify
      unar
      wl-clipboard
      xclip
      xorg.xeyes
      xorg.xrdb
      zathura
    ]);
    darwin = (with stable; [
      reattach-to-user-namespace
    ]) ++ (with custom; [
      gcoreutils
    ]);
  };

  declarative-packages =
    stable.symlinkJoin
      { name = "declarative-packages";
        paths =
          packages.universal ++
          (if stable.stdenv.isLinux then
            packages.linux
          else if stable.stdenv.isDarwin then
            packages.darwin
          else
            []);
      };

  declarative-channels =
    let
      farm =
        stable.linkFarm "declarative-channels-farm"
          (stable.lib.mapAttrsToList
            (name: value: { inherit name; path = value; })
            sources);
    in
      stable.runCommandLocal "declarative-channels" {} ''
        mkdir -p $out
        ln -s ${farm} $out/channels
      '';
in
  { inherit
      declarative-packages
      declarative-channels
    ;
  }

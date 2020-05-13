let
  # Not using pkgs.fetchFromGitHub because it depends on Nixpkgs, which means
  # changing pinned versions rebuilds otherwise unchanged packages.
  fetchGitHub = { owner, repo, rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };

  mkChannel = { rev, sha256 }:
    import (fetchGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      inherit rev sha256;
    }) { config = {}; };

  channels =
    { "nixos-19.09" = mkChannel # ↓ Don't change this revision
        { rev = "856dbd1a5c7fd826cf3668ff12a7389be0686f41";
          sha256 = "1d895i1lc25d2akniaqg2n1jrg2rcd1gih8rpmhyrlv4lpggfmsx";
        };
      "nixos-20.03" = mkChannel
        { rev = "5adf2a6c11646898742b0c08f7e94101620ba707";
          sha256 = "0wf7pwma2qyfak39b242mcq8z7cdj65sds7hcjxchy0448shapzi";
        };
      "nixpkgs-unstable" = mkChannel
        { rev = "6bcb1dec8ea16f20e6404631668cf69e76424eef";
          sha256 = "04x750byjr397d3mfwkl09b2cz7z71fcykhvn8ypxrck8w7kdi1h";
        };
    };

  legacy = channels."nixos-19.09";
  stable = channels."nixos-20.03";
  unstable = channels."nixpkgs-unstable";

  custom =
    { kakoune =
        unstable.kakoune-unwrapped.overrideAttrs (old: rec {
          version = "HEAD";
          src = fetchGitHub {
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
        import (fetchGitHub {
          owner = "target";
          repo = "lorri";
          rev = "896391ed257e6f3cd5bf7a2e802d2761c3be1ff5";
          sha256 = "143ds2cdvxf1sj8g4aw6jaglg719sqb278j6kfclb7q0ykdhirr3";
        }) {};
      ghcide =
        (import (fetchGitHub {
          owner = "cachix";
          repo = "ghcide-nix";
          rev = "f940ec611cc6914693874ee5e024eba921cab19e";
          sha256 = "0vri0rivdzjvxrh6lzlwwkh8kzxsn82jp1c2w5rqzhp87y6g2k8z";
        }) {}).ghcide-ghc865;
      ormolu =
        let
          haskellPackages =
            unstable.haskellPackages.override (old: {
              overrides = haskellPackagesOld: haskellPackagesNew: {
                ghc-lib-parser = haskellPackagesOld.ghc-lib-parser_8_10_1_20200412;
              };
            });
        in
          unstable.haskell.lib.justStaticExecutables haskellPackages.ormolu_0_0_5_0;
      comma =
        import (fetchGitHub {
          owner = "shopify";
          repo = "comma";
          rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
          sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
        }) {};
      iosevka =
        # To install on macOS:
        # 1. cd $(nix-env --query packages --out-path | awk '{print $2}')"/share/fonts/iosevka-pro/"
        # 2. open .
        # 3. Select all
        # 4. Open
        unstable.callPackage (import "${unstable.path}/pkgs/data/fonts/iosevka") {
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
    };

  packages = {
    universal = (with stable; [
    ]) ++ (with unstable; [
      (haskell.lib.justStaticExecutables haskellPackages.cabal-plan)
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
      nodejs
      pandoc
      python3
      ripgrep
      rsync
      rustup
      shellcheck
      skopeo
      stack
      tealdeer
      tectonic
      tmux
      tokei
      universal-ctags
      youtube-dl
    ]) ++ (with custom; [
      # iosevka
      comma
      ghcide
      kakoune
      lorri
      ormolu
    ]);
    linux = (with legacy; [
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
    ]);
  };
in

stable.symlinkJoin {
  name = "packages";
  paths =
    packages.universal ++
    (if stable.stdenv.isLinux then
      packages.linux
    else if stable.stdenv.isDarwin then
      packages.darwin
    else
      []);
}

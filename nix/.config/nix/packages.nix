let
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
    { "nixos-19.09" = mkChannel
        { rev = "856dbd1a5c7fd826cf3668ff12a7389be0686f41";
          sha256 = "1d895i1lc25d2akniaqg2n1jrg2rcd1gih8rpmhyrlv4lpggfmsx";
        };
      "nixos-unstable" = mkChannel
        { rev = "fce7562cf46727fdaf801b232116bc9ce0512049";
          sha256 = "14rvi69ji61x3z88vbn17rg5vxrnw2wbnanxb7y0qzyqrj7spapx";
        };
    };

  stable = channels."nixos-19.09";
  unstable = channels."nixos-unstable";

  custom =
    { kakoune =
        unstable.kakoune-unwrapped.overrideAttrs (old:
          rec {
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
        unstable.callPackage (fetchGitHub {
          owner = "target";
          repo = "lorri";
          rev = "cb966b0d4ab7f4b5861d79a19822eca6b6a50e82";
          sha256 = "1q01cjmvd1shxlwzjsi4gzdn0sx5a132bqql3xksbnhaj7ka6j3f";
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
        stable.callPackage (fetchGitHub {
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
      (haskell.lib.justStaticExecutables haskellPackages.cabal-plan)
      (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
      (haskell.lib.justStaticExecutables haskellPackages.nix-derivation)
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      borgbackup
      cabal-install
      cabal2nix
      cachix
      direnv
      exa
      fd
      fzf
      ghcid
      git
      git-revise
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
      stack
      tealdeer
      tectonic
      tokei
      universal-ctags
      youtube-dl
    ]) ++ (with unstable; [
      dhall
      dhall-json
      fish
      gitAndTools.delta
      hlint
      tmux
    ]) ++ (with custom; [
      comma
      ghcide
      iosevka
      kakoune
      lorri
      ormolu
    ]);
    linux = (with stable; [
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
    darwin = (with unstable; [
      reattach-to-user-namespace
    ]);
  };
in

unstable.symlinkJoin {
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

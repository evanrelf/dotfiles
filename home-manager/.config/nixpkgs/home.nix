{ config, ... }:

let
  mkChannel = { rev, sha256 }:
    import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") { inherit config; };

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
          let
            rev = "c585107ab5e7155f7da648c3752cf360f7156177";
            sha256 = "1rjnhkzwrwxkbi78rpbl06d815jdkpkfpfcv5ppclvpwyqfd98zc";
          in
            rec {
              version = "HEAD";
              src = builtins.fetchTarball {
                url = "https://github.com/mawww/kakoune/archive/${rev}.tar.gz";
                inherit sha256;
              };
              preConfigure = ''
                ${old.preConfigure}
                export version="${version}"
              '';
            });
      lorri =
        let
          rev = "cb966b0d4ab7f4b5861d79a19822eca6b6a50e82";
          sha256 = "1q01cjmvd1shxlwzjsi4gzdn0sx5a132bqql3xksbnhaj7ka6j3f";
        in
          unstable.callPackage (builtins.fetchTarball {
            url = "https://github.com/target/lorri/archive/${rev}.tar.gz";
            inherit sha256;
          }) {};
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
    };

  packages = {
    universal = (with stable; [
      # haskellPackages.hadolint # broken
      # rust-analyzer
      (haskell.lib.justStaticExecutables haskellPackages.cabal-plan)
      (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
      (haskell.lib.justStaticExecutables haskellPackages.nix-derivation)
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      aspell
      bat
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
      gitAndTools.diff-so-fancy
      gitAndTools.hub
      graphviz
      htop
      httpie
      jq
      mosh
      neovim
      nix-diff
      nix-prefetch-git
      nodePackages.prettier
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
      fish
      gitAndTools.delta
      hlint
      tmux
    ]) ++ (with custom; [
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
{
  programs.home-manager.enable = true;
  home.stateVersion = "19.09";
  home.packages =
    packages.universal ++
    (if stable.stdenv.isLinux then
      packages.linux
    else if stable.stdenv.isDarwin then
      packages.darwin
    else
      []);
}

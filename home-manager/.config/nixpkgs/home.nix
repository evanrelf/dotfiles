{ config, ... }:

let
  mkChannel = { rev, sha256 }:
    import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") { inherit config; };

  # Updated 2020-03-30
  channels =
    { "nixos-19.09" = mkChannel
        { rev = "856dbd1a5c7fd826cf3668ff12a7389be0686f41";
          sha256 = "1d895i1lc25d2akniaqg2n1jrg2rcd1gih8rpmhyrlv4lpggfmsx";
        };
      "nixos-unstable" = mkChannel
        { rev = "ae6bdcc53584aaf20211ce1814bea97ece08a248";
          sha256 = "0hjhznns1cxgl3hww2d5si6vhy36pnm53hms9h338v6r633dcy77";
        };
    };

  stable = channels."nixos-19.09";
  unstable = channels."nixos-unstable";

  custom =
    { kakoune = unstable.kakoune-unwrapped.overrideAttrs (old:
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
    };

  packages = {
    universal = (with stable; [
      aspell
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
      haskellPackages.fast-tags
      # haskellPackages.hadolint # broken
      haskellPackages.nix-derivation
      haskellPackages.wai-app-static
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
      hlint
      ormolu
      tmux
    ]) ++ (with custom; [
      kakoune
      lorri
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

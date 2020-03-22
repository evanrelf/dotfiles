{ config, ... }:

let
  mkChannel = name: import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/${name}.tar.gz") { inherit config; };
  stable = mkChannel "nixos-19.09";
  unstable = mkChannel "nixos-unstable";

  # lorri 1.0 (master on 2020-03-02)
  lorri = stable.callPackage (builtins.fetchTarball {
    url = "https://github.com/target/lorri/archive/6ead8867a245de69f218071fa5db9edbd2864613.tar.gz";
    sha256 = "1i1b8iki424ypa49sxnf4q0agxlysvg0cah7hz5996s1kgb7ymc3";
  }) {};

  packages = {
    universal = (with stable; [
      borgbackup
      cabal2nix
      cabal-install
      cachix
      direnv
      exa
      fd
      fish
      fzf
      ghcid
      git
      gitAndTools.diff-so-fancy
      gitAndTools.hub
      git-revise
      haskellPackages.fast-tags
      haskellPackages.wai-app-static
      htop
      httpie
      jq
      mosh
      neovim
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
      yarn
    ]) ++ (with unstable; [
      hlint
      kakoune
      ormolu
      tmux
    ]) ++ [
      lorri
    ];
    linux = with stable; [
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
    ];
    darwin = with stable; [
      reattach-to-user-namespace
    ];
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

{ config, pkgs, ... }:

let
  unstable = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz") { inherit config; };
in
{
  home.packages = (with pkgs; [
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
    kakoune
    ormolu
    tmux
  ]) ++ [
    (pkgs.callPackage (builtins.fetchTarball {
      # lorri 1.0 (master on 2020-03-02)
      url = "https://github.com/target/lorri/archive/6ead8867a245de69f218071fa5db9edbd2864613.tar.gz";
      sha256 = "1i1b8iki424ypa49sxnf4q0agxlysvg0cah7hz5996s1kgb7ymc3";
    }) {})
  ] ++ (if pkgs.stdenv.isLinux then (with pkgs; [
    acpi
    chromium
    dmenu
    feh
    gnupg
    grim
    kitty
    mako
    slurp
    spotify
    wl-clipboard
    xclip
    xorg.xeyes
    xorg.xrdb
  ]) else if pkgs.stdenv.isDarwin then with pkgs; [
    reattach-to-user-namespace
  ] else []);

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "19.09";
}

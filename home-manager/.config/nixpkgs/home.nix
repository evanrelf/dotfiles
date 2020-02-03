{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    autojump
    cabal-install
    cachix
    direnv
    exa
    fd
    fish
    fzf
    ghcid
    git
    git-revise
    gitAndTools.diff-so-fancy
    gitAndTools.hub
    hadolint
    htop
    jq
    kakoune
    lorri
    mosh
    neovim
    nix-prefetch-git
    nodejs
    ormolu
    pandoc
    reattach-to-user-namespace
    ripgrep
    shellcheck
    stack
    tealdeer
    tectonic
    tmux
    tokei
    yarn
  ];

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

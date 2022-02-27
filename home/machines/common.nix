{ pkgs, ... }:

{
  dotfiles.programs = {
    emacs.enable = true;
    fish.enable = true;
    git.enable = true;
    kakoune.enable = true;
    neovim.enable = true;
    nix.enable = true;
    starship.enable = true;
    tmux.enable = true;
  };

  home.packages = with pkgs; [
    as-tree
    bashInteractive
    cabal-install
    cachix
    comma
    coreutils-gprefix
    dhall
    diskus
    fd
    fzf
    ghcid
    haskellPackages.fourmolu
    home-rebuild
    htop
    # TODO: Move off of `nixpkgs-slow`
    slow.httpie
    hyperfine
    iosevka-bin
    jq
    lorri
    lsd
    moreutils
    nerdfonts
    pandoc
    ripgrep
    sd
    shellcheck
    stylish-haskell
    tealdeer
    tokei
    tree
    watchexec
    zoxide
  ];
}

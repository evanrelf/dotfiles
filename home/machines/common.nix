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
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    cabal-install
    cachix
    comma
    coreutils-gprefix
    dhall
    diskus
    fd
    fx
    fzf
    ghcid
    haskellPackages.cabal-plan
    haskellPackages.fourmolu
    haskellPackages.retrie
    home-rebuild
    htop
    httpie
    hyperfine
    iosevka-bin
    janet
    jq
    lorri
    lsd
    moreutils
    nerdfonts
    ormolu
    pandoc
    patat
    ripgrep
    rlwrap
    sd
    shellcheck
    stylish-haskell
    tealdeer
    tokei
    tree
    watchexec
    yj
    zoxide
  ];
}

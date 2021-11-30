{ pkgs, ... }:

{
  dotfiles.programs = {
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
    comma
    coreutils-gprefix
    dhall
    diskus
    exa
    fd
    fx
    fzf
    ghcid
    haskellPackages.cabal-plan
    haskellPackages.fourmolu
    haskellPackages.retrie
    htop
    httpie
    hyperfine
    iosevka-bin
    janet
    jq
    jujutsu
    lorri
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

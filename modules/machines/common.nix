{ pkgs, ... }:

{
  imports = [
    ../programs/fish.nix
    ../programs/git.nix
    ../programs/kakoune.nix
    ../programs/neovim.nix
    ../programs/nix.nix
    ../programs/starship.nix
    ../programs/tmux.nix
  ];

  home.packages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    cabal-install
    cargo
    coreutils-gprefix
    dhall
    diskus
    emacsCustom
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

{ pkgs, ... }:

{ home.stateVersion = "20.03";

  news.display = "silent";

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  home.packages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
    (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
    borgbackup
    cabal-install
    cabal-plan
    cabal2nix
    cachix
    comma
    dhall
    dhall-json
    exa
    fd
    ffmpeg-full
    fish
    fzf
    gcoreutils
    ghcid
    ghcide
    git
    gitAndTools.delta
    gitAndTools.hub
    hlint
    htop
    httpie
    jq
    kakoune
    lorri
    moreutils
    neovim
    nix-diff
    nix-prefetch-git
    nodejs
    ormolu
    pandoc
    python3
    ripgrep
    rustup
    sd
    shellcheck
    tealdeer
    tmux
    tokei
    universal-ctags
    youtube-dl
  ];
}

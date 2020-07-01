{ lib, pkgs, ... }:

{ imports = [
    ./modules/layers/borg
    ./modules/layers/fish
    ./modules/layers/git
    ./modules/layers/hammerspoon
    ./modules/layers/haskell
    ./modules/layers/kakoune
    ./modules/layers/karabiner
    ./modules/layers/kitty
    ./modules/layers/nix
    ./modules/layers/nodejs
    ./modules/layers/tmux
    ./modules/programs/coin.nix
  ];

  home.stateVersion = "20.03";

  news.display = "silent";

  manual.html.enable = true;

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  programs.coin.enable = true;

  layers.fish.enable = true;

  layers.git.enable = true;

  layers.hammerspoon.enable = true;

  layers.haskell.enable = true;

  layers.kakoune.enable = true;

  layers.karabiner.enable = true;

  layers.kitty.enable = true;

  layers.nix.enable = true;

  layers.nodejs.enable = true;

  layers.tmux.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
    dhall
    dhall-json
    exa
    fd
    ffmpeg-full
    fzf
    gcoreutils
    htop
    httpie
    jq
    moreutils
    neovim
    pandoc
    python3
    ripgrep
    rustup
    sd
    shellcheck
    tealdeer
    tokei
    universal-ctags
    youtube-dl
  ];
}

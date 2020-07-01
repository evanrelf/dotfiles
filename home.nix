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

  layers = {
    fish.enable = true;
    git.enable = true;
    hammerspoon.enable = true;
    haskell.enable = true;
    kakoune.enable = true;
    karabiner.enable = true;
    kitty.enable = true;
    nix.enable = true;
    nodejs.enable = true;
    tmux.enable = true;
  };

  home.file =
    let
      enabled = [
        # "doom-emacs"
        # "emacs"
        # "neovim"
        # "nixos"
        # "spacemacs"
        # "sway"
        # "xmonad"
        # "xorg"
      ];

      configs =
        lib.filterAttrs
          (name: _: builtins.elem name enabled)
          (builtins.readDir ./files);

      symlink = name: _: {
        source = ./files + "/${name}";
        target = ".";
        recursive = true;
      };
    in
      lib.mapAttrs symlink configs;

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

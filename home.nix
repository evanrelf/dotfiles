{ lib, pkgs, ... }:

{ imports = [
    ./modules/layers/fish
    ./modules/layers/git
    ./modules/layers/kakoune
    ./modules/layers/kitty
    ./modules/layers/nix
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
    kakoune.enable = true;
    kitty.enable = true;
    nix.enable = true;
    tmux.enable = true;
  };

  home.file =
    let
      enabled = [
        # "borg"
        # "doom-emacs"
        # "emacs"
        "hammerspoon"
        "haskell"
        "karabiner"
        # "neovim"
        # "nixos"
        "npm"
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
    (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
    (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
    borgbackup
    cabal-install
    cabal-plan
    cabal2nix
    dhall
    dhall-json
    exa
    fd
    ffmpeg-full
    fzf
    gcoreutils
    ghcid
    ghcide
    hlint
    htop
    httpie
    jq
    moreutils
    neovim
    nodejs
    ormolu
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

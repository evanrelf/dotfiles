{ lib, pkgs, ... }:

{ imports = [
    ./modules/programs/coin.nix
  ];

  home.stateVersion = "20.03";

  news.display = "silent";

  manual.html.enable = true;

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  programs.coin.enable = true;

  home.file =
    let
      enabled = [
        # "borg"
        # "doom-emacs"
        # "emacs"
        "fish"
        "git"
        "hammerspoon"
        "haskell"
        "kakoune"
        "karabiner"
        "kitty"
        # "neovim"
        "nix"
        # "nixos"
        "npm"
        # "spacemacs"
        # "sway"
        "tmux"
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

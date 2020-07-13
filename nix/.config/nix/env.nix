let
  pkgs = import ./nixpkgs.nix { overlays = [ (import ./overlay.nix) ]; };

in
  pkgs.buildEnv {
    name = "env";
    paths = with pkgs; [
      (aspellWithDicts (d: with d; [ en en-computers en-science ]))
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      dhall
      dhall-json
      exa
      fd
      fish
      fzf
      gcoreutils
      htop
      httpie
      jq
      moreutils
      pandoc
      python3
      ripgrep
      sd
      shellcheck
      tealdeer
      tokei
      universal-ctags
      zoxide

      (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
      cabal-install
      cabal-plan
      cabal2nix
      ghcid
      ghcide
      hlint
      ormolu

      nodejs

      git
      gitAndTools.delta
      gitAndTools.hub

      cachix
      comma
      lorri
      nix
      nix-diff
      nix-prefetch-git
      nix-tree

      cmatrix
      coin
      cowsay
      fortune
      gay
      gti
      lolcat
      pipes
      sl
      toilet

      borgbackup
      ffmpeg-full
      rustup
      youtube-dl

      neovim
      kakoune
      tmux
      nix

      zsh
      racket-minimal
      deno
    ]
    # ++ [
    #   acpi
    #   chromium
    #   dmenu
    #   feh
    #   gnome3.cheese
    #   gnome3.eog
    #   gnome3.evince
    #   gnome3.gnome-boxes
    #   gnome3.gnome-disk-utility
    #   gnome3.gnome-system-monitor
    #   gnome3.nautilus
    #   gnupg
    #   grim
    #   kitty
    #   mako
    #   mpv
    #   slurp
    #   spotify
    #   unar
    #   wl-clipboard
    #   xclip
    #   xorg.xeyes
    #   xorg.xrdb
    #   zathura
    # ]
    ;
  }

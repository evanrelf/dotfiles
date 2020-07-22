let
  pkgs = import ./nixpkgs.nix { overlays = [ (import ./overlay.nix) ]; };

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" {} ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
      ln -s $out/channels/nixpkgs $out/channels/default
    '';

in
  pkgs.buildEnv {
    name = "env";
    paths = with pkgs; [
      (aspellWithDicts (d: with d; [ en en-computers en-science ]))
      (haskell.lib.justStaticExecutables haskellPackages.fast-tags)
      (haskell.lib.justStaticExecutables haskellPackages.wai-app-static)
      borgbackup
      cabal-install
      cabal-plan
      cabal2nix
      cachix
      cmatrix
      coin
      comma
      cowsay
      declarative-channels
      deno
      dhall
      dhall-json
      emacs
      exa
      fd
      ffmpeg-full
      fish
      fortune
      fzf
      gay
      gcoreutils
      ghcid
      # ghcide
      git
      gitAndTools.delta
      gitAndTools.hub
      gti
      hlint
      htop
      httpie
      iosevka-pro
      jq
      kakoune
      lolcat
      lorri
      moreutils
      neovim
      nix
      nix
      nix-diff
      nix-prefetch-git
      nix-tree
      # nodejs
      ormolu
      pandoc
      pipes
      python3
      racket-minimal
      ripgrep
      rust-analyzer
      rustup
      sd
      shellcheck
      sl
      tealdeer
      tmux
      toilet
      tokei
      tree
      universal-ctags
      youtube-dl
      zoxide
      zsh
    ] ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [
      acpi
      dmenu
      firefox
      kitty
      xclip
    ]);
  }

let
  emacsOverlay =
    let
      rev = "43c916bc555d9531142e1b5e912b4c7639dde916";
    in
      import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
        sha256 = "0yqqkmy006hjm4ji7q0q99d1z413pxk9mb74915rmhnl8h43ak1l";
      });

  myOverlay = import ./overlay.nix;

  pkgs = import ./nixpkgs.nix { overlays = [ emacsOverlay myOverlay ]; };

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
      cabal-install
      comma
      declarative-channels
      exa
      fd
      findutils
      fish
      fzf
      gcoreutils
      ghcid
      git
      gitAndTools.delta
      jq
      kakoune
      neovim
      nix-index
      nix-prefetch-git
      ormolu
      ripgrep
      rust-analyzer
      rustup
      sd
      shellcheck
      tealdeer
      tmux
      zoxide
    ] ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [
      acpi
      autocutsel
      chromium
      dmenu
      emacsGccVterm
      firefox
      iosevka-pro
      kitty
      redshift
      spotify
      xbanish
      xclip
    ]);
  }

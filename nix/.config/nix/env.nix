{ hostname }:

let
  pkgs = import ./nixpkgs.nix { overlays = import ./overlays; };

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
      direnv
      exa
      fd
      fish
      fourmolu
      fzf
      gcoreutils
      gfind
      ggrep
      ghcid
      git
      gitAndTools.delta
      jq
      kakoune
      lorri
      moreutils
      neovim
      nix-diff
      nix-direnv
      nix-index
      nix-prefetch-git
      nix-top
      nix-tree
      ormolu
      pandoc
      ripgrep
      sd
      shellcheck
      tealdeer
      teip
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
    ]) ++ (pkgs.lib.optionals (builtins.elem hostname [ "auburn" "sienna" ]) [
      borgbackup
      idris2
      rclone
      rust-analyzer
      rustup
    ]);
  }

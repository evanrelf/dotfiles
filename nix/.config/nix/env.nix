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
      borgbackup
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
      graphviz # For org-roam
      jq
      kakoune
      neovim
      nix-index
      nix-prefetch-git
      ormolu
      rclone
      ripgrep
      rust-analyzer
      rustup
      sd
      shellcheck
      sqlite # For org-roam
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

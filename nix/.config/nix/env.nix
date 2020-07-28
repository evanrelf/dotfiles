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
      cabal-install
      comma
      declarative-channels
      exa
      fd
      fish
      fzf
      gcoreutils
      ghcid
      git
      gitAndTools.delta
      jq
      kakoune
      neovim
      nix-prefetch-git
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
      dmenu
      emacs
      firefox
      iosevka-pro
      kitty
      xclip
    ]);
  }

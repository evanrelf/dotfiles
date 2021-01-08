{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr $(hostname -s)`)"
}:

let
  pkgs = import ./pkgs.nix {};

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" {} ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
      ln -s $out/channels/nixpkgs $out/channels/default
    '';

  isPersonalMachine =
    if builtins.elem hostname [ "auburn" "sienna" ] then
      builtins.trace
        "Installing extra packages for personal machine '${hostname}'"
        true
    else
      false;

  isWorkMachine =
    if builtins.elem hostname [ "indigo" ] then
      builtins.trace
        "Installing extra packages for work machine '${hostname}'"
        true
    else
      false;

in
  pkgs.buildEnv {
    name = "env";
    paths = with pkgs; [
      (aspellWithDicts (d: with d; [ en en-computers en-science ]))
      bash
      cabal-install
      comma
      coreutils-gprefix
      declarative-channels
      dhall
      direnv
      exa
      fd
      findutils-gprefix
      fish
      fourmolu
      fzf
      gawk-gprefix
      ghcid
      git
      gitAndTools.delta
      gnugrep-gprefix
      httpie
      jq
      kak-lsp
      kakoune
      lorri
      man-gprefix
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
      tmux-xpanes
      vis
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
    ]) ++ (pkgs.lib.optionals isPersonalMachine [
      borgbackup
      idris2
      rclone
      rust-analyzer
      rustup
    ]) ++ (pkgs.lib.optionals isWorkMachine [
    ]);
  }

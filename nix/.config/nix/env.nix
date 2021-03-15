{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr hostname $(hostname -s)`)"
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
    if builtins.elem hostname [ "indigo" "azure" ] then
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
      bashInteractive
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
      ghcid
      gitAndTools.delta
      gitAndTools.gh
      gitAndTools.git-absorb
      gitAndTools.gitFull
      gnugrep-gprefix
      httpie
      jq
      kakoune
      lorri
      magic-wormhole
      moreutils
      neovim
      nix-diff
      nix-index
      nix-top
      nix-tree
      ormolu
      pandoc
      ripgrep
      sd
      shellcheck
      starship
      tealdeer
      tmux
      yj
      zoxide
    ] ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [
      acpi
      autocutsel
      chromium
      dmenu
      firefox
      iosevka-pro
      kitty
      redshift
      spotify
      xbanish
      xclip
    ]) ++ (pkgs.lib.optionals isPersonalMachine [
      borgbackup
      mercurialFull
      rclone
    ]) ++ (pkgs.lib.optionals isWorkMachine [
      tmux-xpanes
    ]);
  }

{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr hostname $(hostname -s)`)"
}:

let
  pkgs = import ./pkgs.nix {};

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" {} ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
    '';

  isLinux =
    if pkgs.stdenv.isLinux then
      builtins.trace
        "Installing extra packages for Linux machine"
        true
    else
      false;

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
      as-tree
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
      gitAndTools.gitFull
      gnugrep-gprefix
      htop
      httpie
      hyperfine
      jq
      kakoune
      lorri
      magic-wormhole
      moreutils
      neovim
      nix-diff
      nix-index
      nixpkgs-fmt
      nix-prefetch-git
      nix-top
      nix-tree
      ormolu
      pandoc
      perlPackages.GitAutofixup
      ripgrep
      sd
      shellcheck
      starship
      tealdeer
      tmux
      yj
      zoxide
    ] ++ (pkgs.lib.optionals isLinux [
      acpi
      autocutsel
      bemenu
      chromium
      dmenu # keep for `dmenu_path | bemenu`
      firefox
      iosevka-bin
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

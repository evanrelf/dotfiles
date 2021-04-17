{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr hostname $(hostname -s)`)"
}:

let
  pkgs = import ./pkgs.nix { };

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
    '';

  traceIf = bool: message:
    if bool then builtins.trace message true else false;

  isLinux =
    traceIf
      pkgs.stdenv.isLinux
      "Installing extra packages for Linux machine";

  isPersonalMachine =
    traceIf
      (builtins.elem hostname [ "auburn" "sienna" ])
      "Installing extra packages for personal machine '${hostname}'";

  isWorkMachine =
    traceIf
      (builtins.elem hostname [ "indigo" ])
      "Installing extra packages for work machine '${hostname}'";

in
pkgs.buildEnv {
  name = "env";
  paths = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    cabal-install
    coreutils-gprefix
    declarative-channels
    dhall
    direnv
    exa
    fd
    findutils-gprefix
    fish
    fzf
    ghcid
    gitAndTools.delta
    gitAndTools.gh
    gitAndTools.gitFull
    gnugrep-gprefix
    haskellPackages.fourmolu
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
    tree
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
    go
    mercurialFull
    rclone
  ]) ++ (pkgs.lib.optionals isWorkMachine [
    tmux-xpanes
  ]);
}

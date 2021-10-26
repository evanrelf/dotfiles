{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr hostname $(hostname -s)`)"
}:

# To install fonts on macOS:
# $ open ~/.nix-profile/share/fonts/truetype/**/*.{ttf,ttc}

let
  pkgs = import ./pkgs.nix { };

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
    '';

  commonPackages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    cabal-install
    coreutils-gprefix
    declarative-channels
    delta
    dhall
    direnv
    exa
    fd
    # findutils-gprefix
    fish
    fx
    fzf
    gitAndTools.delta
    gitAndTools.gh
    gitAndTools.gitFull
    # gnugrep-gprefix
    haskellPackages.cabal-plan
    haskellPackages.fourmolu
    haskellPackages.retrie
    helix
    htop
    httpie
    hyperfine
    iosevka-bin
    janet
    jq
    kakoune
    lorri
    moreutils
    neovim
    nerdfonts
    nix-diff
    nixpkgs-fmt
    nix-prefetch-git
    nix-top
    nix-tree
    pandoc
    patat
    perlPackages.GitAutofixup
    ripgrep
    rlwrap
    sd
    shellcheck
    starship
    stylish-haskell
    tealdeer
    tmux
    tokei
    tree
    yj
    zoxide
  ];

  x86_64Packages = with pkgs; [
    emacsCustom
    ghcid
    magic-wormhole
    nix-index
    ormolu
    watchexec
  ];

  personalPackages = with pkgs; [
    borgbackup
    ffmpeg
    rclone
    youtube-dl
  ];

  linuxPackages = with pkgs; [
    acpi
    autocutsel
    bemenu
    dmenu # keep for `dmenu_path | bemenu`
    firefox
    kitty
    noto-fonts
    redshift
    spotify
    xbanish
    xclip
  ];

in
pkgs.buildEnv {
  name = "env";
  paths = pkgs.lib.concatLists (builtins.getAttr hostname {
    "auburn" = [
      commonPackages
      x86_64Packages
      personalPackages
    ];

    "ultraviolet" = [
      commonPackages
      personalPackages
    ];

    "sienna" = [
      commonPackages
      x86_64Packages
      personalPackages
      linuxPackages
    ];

    "indigo" = [
      commonPackages
      x86_64Packages
      [ pkgs.tmux-xpanes ]
    ];

    "hydra-dev" = [
      commonPackages
      x86_64Packages
      [
        pkgs.gcc
        pkgs.gnupg
        pkgs.pinentry-curses
      ]
    ];
  });
}

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
    cabal-install
    declarative-channels
    delta
    dhall
    exa
    fd
    fish
    fzf
    ghcid
    gitAndTools.gitFull
    haskellPackages.fourmolu
    jq
    kakoune
    lorri
    neovim
    nix-diff
    nixpkgs-fmt
    nix-tree
    patat
    ripgrep
    sd
    shellcheck
    starship
    tealdeer
    tmux
    zig
    zoxide
  ];

  extraPackages = with pkgs; [
    # emacsCustom
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    # coreutils-gprefix
    direnv
    # findutils-gprefix
    gitAndTools.delta
    gitAndTools.gh
    # gnugrep-gprefix
    haskellPackages.cabal-plan
    haskellPackages.retrie
    htop
    httpie
    hyperfine
    iosevka-bin
    magic-wormhole
    moreutils
    nerdfonts
    nix-index
    nix-prefetch-git
    nix-top
    # ormoloog
    ormolu
    pandoc
    parinfer-rust
    perlPackages.GitAutofixup
    rlwrap
    stylish-haskell
    # tmux-thumbs
    tokei
    tree
    watchexec
    yj
  ];

  personalPackages = with pkgs; [
    borgbackup
    ffmpeg
    go
    mercurialFull
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
      extraPackages
      personalPackages
    ];

    "sienna" = [
      commonPackages
      extraPackages
      personalPackages
      linuxPackages
    ];

    "indigo" = [
      commonPackages
      extraPackages
      [ pkgs.tmux-xpanes ]
    ];

    "hydra-dev" = [
      commonPackages
      [
        pkgs.gcc
        pkgs.gnupg
        pkgs.pinentry-curses
      ]
    ];
  });
}

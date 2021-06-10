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

  essentialPackages = with pkgs; [
    cabal-install
    declarative-channels
    delta
    exa
    fd
    fish
    fzf
    ghcid
    gitAndTools.gitFull
    jq
    kakoune
    nix-diff
    ripgrep
    sd
    shellcheck
    starship
    tealdeer
    tmux
    zoxide
  ];

  extraPackages = with pkgs; [
    # emacsCustom
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    coreutils-gprefix
    dhall
    direnv
    findutils-gprefix
    gitAndTools.delta
    gitAndTools.gh
    gnugrep-gprefix
    haskellPackages.cabal-plan
    haskellPackages.fourmolu
    helix
    htop
    httpie
    hyperfine
    iosevka-bin
    lorri
    magic-wormhole
    moreutils
    neovim
    nerdfonts
    nix-index
    nix-prefetch-git
    nix-top
    nix-tree
    nixpkgs-fmt
    ormoloog
    ormolu
    pandoc
    parinfer-rust
    perlPackages.GitAutofixup
    rlwrap
    stylish-haskell
    tmux-thumbs
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
      essentialPackages
      extraPackages
      personalPackages
    ];

    "sienna" = [
      essentialPackages
      extraPackages
      personalPackages
      linuxPackages
    ];

    "indigo" = [
      essentialPackages
      extraPackages
      [ pkgs.tmux-xpanes ]
    ];

    "hydra-dev" = [
      essentialPackages
    ];
  });
}

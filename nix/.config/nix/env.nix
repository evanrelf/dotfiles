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
    # emacsCustom
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
    haskellPackages.cabal-plan
    haskellPackages.fourmolu
    helix
    htop
    httpie
    hyperfine
    iosevka-bin
    jq
    kakoune
    lorri
    magic-wormhole
    moreutils
    neovim
    nerdfonts
    nix-diff
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
    ripgrep
    rlwrap
    sd
    shellcheck
    starship
    stylish-haskell
    tealdeer
    tmux
    tmux-thumbs
    tokei
    tree
    watchexec
    yj
    zoxide
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
      personalPackages
    ];

    "sienna" = [
      commonPackages
      personalPackages
      linuxPackages
    ];

    "indigo" = [
      commonPackages
      [ pkgs.tmux-xpanes ]
    ];
  });
}

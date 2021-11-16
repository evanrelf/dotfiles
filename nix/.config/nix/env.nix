{ hostname ? throw "Please specify your machine's hostname (e.g. `--argstr hostname $(hostname -s)`)"
}:

# To install fonts on macOS:
# $ open ~/.nix-profile/share/fonts/truetype/**/*.{ttf,ttc}

let
  pkgs = import ./pkgs.nix { };

  pkgs-x86_64 = import ./pkgs.nix { localSystem = "x86_64-darwin"; };

  useRosetta = attr:
    if builtins.currentSystem == "aarch64-darwin" then
      pkgs-x86_64."${attr}"
    else
      pkgs."${attr}";

  declarative-channels =
    pkgs.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
    '';

  commonPackages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    (useRosetta "ghcid")
    (useRosetta "nix-index")
    (useRosetta "ormolu")
    (useRosetta "watchexec")
    as-tree
    bashInteractive
    cabal-install
    cargo
    coreutils-gprefix
    declarative-channels
    delta
    dhall
    direnv
    diskus
    emacsCustom
    exa
    fd
    fish
    fx
    fzf
    git-branchless
    gitAndTools.delta
    gitAndTools.gh
    gitAndTools.gitFull
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
    rustc
    rust-script
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
      personalPackages
    ];

    "ultraviolet" = [
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

{ system
, inputs
}:

# To install fonts on macOS:
# $ open ~/.nix-profile/share/fonts/truetype/**/*.{ttf,ttc}

let
  pkgs =
    import inputs.nixpkgs {
      inherit system;
      inherit (inputs.self) overlays;
    };

  pkgs-x86_64 =
    import inputs.nixpkgs {
      system = "x86_64-darwin";
      inherit (inputs.self) overlays;
    };

  useRosetta = attr:
    if system == "aarch64-darwin" then
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

  buildEnvWith = packageSets:
    pkgs.buildEnv {
      name = "env";
      paths = pkgs.lib.concatLists packageSets;
    };

in
{
  "auburn" = buildEnvWith [
    commonPackages
    personalPackages
  ];

  "ultraviolet" = buildEnvWith [
    commonPackages
    personalPackages
  ];

  "sienna" = buildEnvWith [
    commonPackages
    personalPackages
    linuxPackages
  ];

  "indigo" = buildEnvWith [
    commonPackages
    [ pkgs.tmux-xpanes ]
  ];

  "hydra-dev" = buildEnvWith [
    commonPackages
    [
      pkgs.gcc
      pkgs.gnupg
      pkgs.pinentry-curses
    ]
  ];
}

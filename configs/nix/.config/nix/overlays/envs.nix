pkgsFinal: pkgsPrev:

# To install fonts on macOS:
# $ open ~/.nix-profile/share/fonts/truetype/**/*.{ttf,ttc}

let
  declarative-channels =
    pkgsPrev.runCommandLocal "declarative-channels" { } ''
      mkdir -p $out/channels
      ln -s ${pkgsFinal.path} $out/channels/nixpkgs
    '';

  commonPackages = with pkgsFinal; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
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
    rosetta.ghcid
    rosetta.nix-index
    rosetta.ormolu
    rosetta.watchexec
    sd
    shellcheck
    stylish-haskell
    tealdeer
    tmux
    tokei
    tree
    yj
    zoxide
  ];

  personalPackages = with pkgsFinal; [
    borgbackup
    ffmpeg
    rclone
    youtube-dl
  ];

  linuxPackages = with pkgsFinal; [
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

  buildEnvs = attrs:
    pkgsPrev.lib.mapAttrs
      (hostname: packageSets:
        pkgsPrev.buildEnv {
          name = "env";
          paths = pkgsPrev.lib.concatLists packageSets;
        })
      attrs;


in
{
  rosetta =
    import pkgsFinal.path {
      system =
        if pkgsFinal.system == "aarch64-darwin" then
          "x86_64-darwin"
        else
          pkgsFinal.system;

      inherit (pkgsFinal) overlays;
    };

  envs = buildEnvs {
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
      [ pkgsFinal.tmux-xpanes ]
    ];

    "hydra-dev" = [
      commonPackages
      [
        pkgsFinal.gcc
        pkgsFinal.gnupg
        pkgsFinal.pinentry-curses
      ]
    ];
  };
}

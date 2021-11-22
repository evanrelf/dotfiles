pkgsFinal: pkgsPrev:

# To install fonts on macOS:
# $ open ~/.nix-profile/share/fonts/truetype/**/*.{ttf,ttc}

let
  commonPackages = with pkgsFinal; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    as-tree
    bashInteractive
    cabal-install
    cargo
    coreutils-gprefix
    dhall
    diskus
    emacsCustom
    exa
    fd
    fx
    fzf
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
    lorri
    moreutils
    neovim
    nerdfonts
    pandoc
    patat
    ripgrep
    rlwrap
    rosetta.ghcid
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
    ];

    "hydra-dev" = [
      commonPackages
    ];
  };
}

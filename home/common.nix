{ config, inputs, lib, pkgs, ... }:

{
  home.stateVersion = "22.11";

  home.enableNixpkgsReleaseCheck = true;

  news.display = "silent";

  home.packages = lib.foldlAttrs (xs: _: ys: xs ++ ys) [ ] {
    data = [
      pkgs.duckdb
      pkgs.ijq
      pkgs.jless
      pkgs.jq
      pkgs.prqlc
      pkgs.sqlite
    ];

    editors = [
      pkgs.helix
      pkgs.kakoune
      pkgs.neovim
      pkgs.parinfer-rust
    ];

    fonts = [
      pkgs.iosevka-bin
      pkgs.nerdfonts
    ];

    git = [
      pkgs.delta
      pkgs.git
      pkgs.git-revise
      pkgs.jujutsu
      pkgs.prr
    ];

    gnu = [
      pkgs.coreutils-gprefix
      pkgs.findutils-gprefix
      pkgs.gawkInteractive-gprefix
      pkgs.gnugrep-gprefix
      pkgs.gnused-gprefix
    ];

    haskell = [
      pkgs.haskellPackages.hiedb
    ];

    nix = [
      pkgs.comma
      pkgs.comma-update
      pkgs.direnv
      pkgs.home-manager
      pkgs.home-rebuild
      pkgs.nix-diff
      pkgs.nix-direnv
      pkgs.nix-tree
      pkgs.nix-your-shell
      pkgs.nixpkgs-fmt
    ];

    rust = [
      pkgs.cargo-edit
      pkgs.cargo-expand
      pkgs.cargo-flamegraph
      pkgs.cargo-limit
      pkgs.cargo-watch
      pkgs.rustup
    ];

    shell = [
      pkgs.babelfish
      pkgs.fish
      pkgs.shellcheck
      pkgs.starship
      pkgs.tealdeer
      pkgs.zoxide
    ];

    virtualization = [
      pkgs.colima
      pkgs.docker-client
      pkgs.lima-bin
    ];

    uncategorized = [
      pkgs.deno
      pkgs.fd
      pkgs.frawk
      pkgs.fzf
      pkgs.graphviz
      pkgs.htop
      pkgs.httpie
      pkgs.hyperfine
      pkgs.ollama
      pkgs.pandoc
      pkgs.pstree
      pkgs.ripgrep
      pkgs.sd
      pkgs.tectonic
      pkgs.tree
      pkgs.ugrep
      pkgs.universal-ctags
      pkgs.watchexec
      pkgs.watchman
      pkgs.yt-dlp
      pkgs.zf
    ];
  };

  # Copying font files on Darwin is too slow
  home.activation.copyFonts = lib.mkForce "true";

  xdg.configFile."direnv" = {
    source = ../configs/direnv/.config/direnv;
    recursive = true;
  };

  programs.fish.enable = true;

  programs.fish.plugins = [
    {
      name = "fzf";
      src = pkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "fzf";
        rev = "479fa67d7439b23095e01b64987ae79a91a4e283";
        hash = "sha256-28QW/WTLckR4lEfHv6dSotwkAKpNJFCShxmKFGQQ1Ew=";
      };
    }
    {
      name = "fish-colored-man";
      src = pkgs.fetchFromGitHub {
        owner = "decors";
        repo = "fish-colored-man";
        rev = "1ad8fff696d48c8bf173aa98f9dff39d7916de0e";
        hash = "sha256-uoZ4eSFbZlsRfISIkJQp24qPUNqxeD0JbRb/gVdRYlA=";
      };
    }
  ];

  programs.fish.shellInit = ''
    set --global --export NIX_PATH "nixpkgs=${inputs.nixpkgs}"
  '';

  xdg.configFile."fish" = {
    source = ../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."fish/home-manager.fish".source =
    config.xdg.configFile."fish/config.fish".source;

  xdg.configFile."ghostty" = {
    source = ../configs/ghostty/.config/ghostty;
    recursive = true;
  };

  xdg.configFile."git" = {
    source = ../configs/git/.config/git;
    recursive = true;
  };

  xdg.configFile."hammerspoon" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../configs/hammerspoon/.config/hammerspoon;
    recursive = true;
  };

  xdg.configFile."helix" = {
    source = ../configs/helix/.config/helix;
    recursive = true;
  };

  xdg.configFile."jj" = {
    source = ../configs/jujutsu/.config/jj;
    recursive = true;
  };

  xdg.configFile."karabiner" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../configs/karabiner/.config/karabiner;
    recursive = true;
  };

  xdg.configFile."kak" = {
    source = ../configs/kakoune/.config/kak;
    recursive = true;
  };

  xdg.configFile."kak/autoload/runtime".source =
    "${pkgs.kakoune}/share/kak/autoload";

  xdg.configFile."nvim" = {
    source = ../configs/neovim/.config/nvim;
    recursive = true;
  };

  xdg.configFile."ripgrep" = {
    source = ../configs/ripgrep/.config/ripgrep;
    recursive = true;
  };

  xdg.configFile."starship.toml".source =
    ../configs/starship/.config/starship.toml;

  xdg.configFile."wezterm" = {
    source = ../configs/wezterm/.config/wezterm;
    recursive = true;
  };
}

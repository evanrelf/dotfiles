{ config, inputs, lib, pkgs, ... }:

let
  mutable = path:
    config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/Code/evanrelf/dotfiles/${path}";

in
{
  home.stateVersion = "22.11";

  home.enableNixpkgsReleaseCheck = true;

  news.display = "silent";

  home.packages = [
    pkgs.babelfish
    pkgs.cargo-edit
    pkgs.cargo-limit
    pkgs.cargo-watch
    pkgs.colima
    pkgs.coreutils-gprefix
    pkgs.delta
    pkgs.direnv
    pkgs.docker-client
    pkgs.duckdb
    pkgs.erlang
    pkgs.fd
    pkgs.findutils-gprefix
    pkgs.fish
    pkgs.fzf
    pkgs.gawkInteractive-gprefix
    pkgs.gh
    pkgs.git
    pkgs.git-confirm-push
    pkgs.git-lookup
    pkgs.gleam
    pkgs.gnugrep-gprefix
    pkgs.gnused-gprefix
    pkgs.graphviz
    pkgs.gum
    pkgs.helix
    pkgs.home-manager
    pkgs.home-rebuild
    pkgs.httpie
    pkgs.jj-lookup
    pkgs.jq
    pkgs.jujutsu
    pkgs.kakoune
    pkgs.kakoune-lsp
    # pkgs.kitty
    pkgs.lima-bin
    pkgs.llm
    pkgs.marimo
    pkgs.neovim
    pkgs.nix-direnv
    pkgs.nix-your-shell
    pkgs.nixpkgs-fmt
    pkgs.python3
    pkgs.rebar3
    pkgs.ripgrep
    # pkgs.roc
    pkgs.ruff
    pkgs.rust-script
    pkgs.rustup
    pkgs.sd
    pkgs.shellcheck
    pkgs.sqlite
    pkgs.starship
    pkgs.tealdeer
    pkgs.tree
    pkgs.universal-ctags
    pkgs.uv
    pkgs.watchexec
    pkgs.watchman
    pkgs.zigpkgs."0.13.0"
    pkgs.zoxide
  ];

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

  xdg.configFile."kak-lsp" = {
    source = ../configs/kakoune/.config/kak-lsp;
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

  home.file.".config/zed/settings.json".source =
    mutable "configs/zed/.config/zed/settings.json";
}

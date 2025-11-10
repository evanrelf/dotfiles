{ config, inputs, lib, pkgs, ... }:

let
  dotfiles =
    "${config.home.homeDirectory}/Code/evanrelf/dotfiles";

  mutable = path:
    config.lib.file.mkOutOfStoreSymlink "${dotfiles}/${path}";

in
{
  home.stateVersion = "22.11";

  home.enableNixpkgsReleaseCheck = true;

  news.display = "silent";

  home.packages = with pkgs; [
    as-tree
    bat
    cargo-edit
    cargo-limit
    cargo-watch
    claude-code
    codex
    coreutils-gprefix
    delta
    direnv
    docker-client
    duckdb
    empath
    fd
    findutils-gprefix
    fish
    fzf
    gawkInteractive-gprefix
    git
    gnugrep-gprefix
    gnused-gprefix
    graphviz
    gron
    gum
    home-manager
    hsl
    httpie
    hyperfine
    jq
    jujutsu
    kakoune
    kakoune-lsp
    lima
    mergiraf
    moreutils
    nix-direnv
    nix-your-shell
    nixpkgs-fmt
    pancase
    pandoc
    ripgrep
    ruff
    rustup
    samply
    sd
    shellcheck
    sqlite-interactive
    starship
    tealdeer
    tokei
    tree
    universal-ctags
    uv
    watchexec
    watchman
    zoxide
  ];

  # Copying font files on Darwin is too slow
  home.activation.copyFonts = lib.mkForce "true";

  home.file.".claude/settings.json".source =
    mutable "configs/claude/.claude/settings.json";

  xdg.configFile."direnv" = {
    source = ../../configs/direnv/.config/direnv;
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
    set --global --export --prepend PATH "${dotfiles}/bin"
    set --global --export NIX_PATH "nixpkgs=${inputs.nixpkgs}"
  '';

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."fish/home-manager.fish".source =
    config.xdg.configFile."fish/config.fish".source;

  home.file.".config/ghostty/config".source =
    mutable "configs/ghostty/.config/ghostty/config";

  xdg.configFile."ghostty/themes" = {
    source = ../../configs/ghostty/.config/ghostty/themes;
    recursive = true;
  };

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
    recursive = true;
  };

  home.file.".config/hammerspoon/init.lua".source =
    mutable "configs/hammerspoon/.config/hammerspoon/init.lua";

  home.file.".config/jj/config.toml".source =
    mutable "configs/jj/.config/jj/config.toml";

  xdg.configFile."karabiner" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../../configs/karabiner/.config/karabiner;
    recursive = true;
  };

  home.file.".config/kak/kakrc".source =
    mutable "configs/kakoune/.config/kak/kakrc";

  home.file.".config/kak/colors".source =
    mutable "configs/kakoune/.config/kak/colors";

  xdg.configFile."kak-lsp" = {
    source = ../../configs/kakoune/.config/kak-lsp;
    recursive = true;
  };

  xdg.configFile."kak/autoload/runtime".source =
    "${pkgs.kakoune}/share/kak/autoload";

  xdg.configFile."ripgrep" = {
    source = ../../configs/ripgrep/.config/ripgrep;
    recursive = true;
  };

  xdg.configFile."starship.toml".source =
    mutable "configs/starship/.config/starship.toml";

  home.file.".config/zed/settings.json".source =
    mutable "configs/zed/.config/zed/settings.json";
}

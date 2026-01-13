{ config, inputs, lib, pkgs, ... }:

let
  dotfiles =
    "${config.home.homeDirectory}/Code/evanrelf/dotfiles";

  mutable = path:
    config.lib.file.mkOutOfStoreSymlink "${dotfiles}/${path}";

in
{
  # Home Manager

  home.enableNixpkgsReleaseCheck = true;

  news.display = "silent";

  home.username = "evanrelf";

  home.homeDirectory = lib.mkMerge [
    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin "/Users/${config.home.username}")
    (lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) "/home/${config.home.username}")
  ];

  # Packages

  home.packages = with pkgs; [
    as-tree
    bat
    cargo-edit
    cargo-limit
    cargo-watch
    claude-code
    colima
    coreutils-gprefix
    delta
    direnv
    docker-client
    empath
    evanrelf-fish
    evanrelf-prompt
    fd
    findutils-gprefix
    fzf
    gawkInteractive-gprefix
    git
    gnugrep-gprefix
    gnused-gprefix
    home-manager
    hsl
    hyperfine
    jq
    jujutsu
    kakoune
    kakoune-lsp
    lima
    magika-cli
    mergiraf
    nix-diff
    nix-direnv
    nix-your-shell
    nixpkgs-fmt
    pancase
    pueue
    ripgrep
    rustup
    samply
    scc
    sd
    shellcheck
    tealdeer
    tombi
    universal-ctags
    up
    uutils-coreutils
    watchexec
    watchman
    zig
    zoxide
  ];

  # Config files

  home.file.".claude/settings.json".source =
    mutable "configs/claude/.claude/settings.json";

  home.file.".claude/CLAUDE.md".source =
    mutable "configs/claude/.claude/CLAUDE.md";

  xdg.configFile."direnv" = {
    source = ../../configs/direnv/.config/direnv;
    recursive = true;
  };

  xdg.configFile."fd" = {
    source = ../../configs/fd/.config/fd;
    recursive = true;
  };

  home.file.".config/fish/config.fish".source =
    mutable "configs/fish/.config/fish/config.fish";

  home.file.".config/fish/conf.d".source =
    mutable "configs/fish/.config/fish/conf.d";

  home.file.".config/fish/functions".source =
    mutable "configs/fish/.config/fish/functions";

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."fish/home-manager.fish".text = ''
    set --global --export DOTFILES "${dotfiles}"
    set --global --export NIX_PATH "nixpkgs=${inputs.nixpkgs}"
  '';

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

  xdg.configFile."kak/autoload/runtime".source =
    "${pkgs.kakoune}/share/kak/autoload";

  xdg.configFile."ripgrep" = {
    source = ../../configs/ripgrep/.config/ripgrep;
    recursive = true;
  };

  home.file.".config/zed/settings.json".source =
    mutable "configs/zed/.config/zed/settings.json";

  # Services

  services.colima.enable = true;

  services.pueue.enable = true;
}

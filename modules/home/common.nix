{ config, inputs, lib, pkgs, ... }:

let
  dotfiles =
    "${config.home.homeDirectory}/Code/evanrelf/dotfiles";

  mutable = path:
    config.lib.file.mkOutOfStoreSymlink "${dotfiles}/${path}";

in
{
  # Home Manager

  home.stateVersion = "22.11";

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
    codex
    coreutils-gprefix
    delta
    direnv
    empath
    evanrelf-fish
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
    least
    lima
    mergiraf
    nix-direnv
    nix-your-shell
    nixpkgs-fmt
    pancase
    pueue
    ripgrep
    rustup
    samply
    sd
    shellcheck
    starship
    tealdeer
    tokei
    universal-ctags
    up
    watchexec
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

  # Services

  services.pueue.enable = true;
}

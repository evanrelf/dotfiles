{ config, inputs, lib, pkgs, ... }:

{
  home.stateVersion = "22.11";

  home.enableNixpkgsReleaseCheck = true;

  home.packages = [
    pkgs.cached-nix-shell
    pkgs.cargo-edit
    pkgs.cargo-flamegraph
    pkgs.cargo-limit
    pkgs.cargo-watch
    pkgs.comma
    pkgs.comma-update
    pkgs.coreutils-gprefix
    pkgs.delta
    pkgs.direnv
    pkgs.emacs
    pkgs.fd
    pkgs.findutils-gprefix
    pkgs.fzf
    pkgs.git
    pkgs.gnugrep-gprefix
    pkgs.home-manager
    pkgs.home-rebuild
    pkgs.ijq
    pkgs.iosevka-bin
    pkgs.jq
    pkgs.kakoune
    pkgs.neovim
    pkgs.nerdfonts
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.qsv
    pkgs.ripgrep
    pkgs.rustup
    pkgs.sd
    pkgs.shellcheck
    pkgs.starship
    pkgs.stylua
    pkgs.tealdeer
    pkgs.zoxide
  ];

  xdg.configFile."doom" = {
    source = ../configs/emacs/.config/doom;
    recursive = true;
  };

  home.file.".local/bin" = {
    source = ../configs/emacs/.local/bin;
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

  xdg.configFile."fish/home-manager.fish".text =
    config.xdg.configFile."fish/config.fish".text;

  xdg.configFile."git" = {
    source = ../configs/git/.config/git;
    recursive = true;
  };

  xdg.configFile."hammerspoon" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../configs/hammerspoon/.config/hammerspoon;
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

  xdg.configFile."nvim" = {
    source = ../configs/neovim/.config/nvim;
    recursive = true;
  };

  xdg.configFile."starship.toml".source =
    ../configs/starship/.config/starship.toml;

  xdg.configFile."wezterm" = {
    source = ../configs/wezterm/.config/wezterm;
    recursive = true;
  };
}

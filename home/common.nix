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
    pkgs.frawk
    pkgs.fzf
    pkgs.gawkInteractive-gprefix
    pkgs.git
    pkgs.git-revise
    pkgs.gnugrep-gprefix
    pkgs.gnused-gprefix
    pkgs.helix
    pkgs.home-manager
    pkgs.home-rebuild
    pkgs.ijq
    pkgs.iosevka-bin
    pkgs.jq
    pkgs.jujutsu
    pkgs.kakoune
    pkgs.neovim
    pkgs.nerdfonts
    pkgs.nil
    pkgs.nix-direnv
    pkgs.nix-your-shell
    pkgs.nixpkgs-fmt
    pkgs.pandoc
    # pkgs.qsv
    pkgs.racket
    pkgs.ripgrep
    pkgs.rustup
    pkgs.sd
    pkgs.shellcheck
    pkgs.starship
    pkgs.stylua
    pkgs.tealdeer
    pkgs.watchexec
    pkgs.zoxide
  ];

  # Copying font files on Darwin is too slow
  home.activation.copyFonts = lib.mkForce "true";

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

  xdg.configFile."fish/home-manager.fish".source =
    config.xdg.configFile."fish/config.fish".source;

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
    onChange = ''
      $DRY_RUN_CMD rm -f "$HOME/.config/nvim/plugin/packer_compiled.lua"
      $DRY_RUN_CMD rm -rf "$HOME/.cache/nvim/hotpot/"
    '';
  };

  xdg.dataFile."nvim/site/pack/home-manager/start/packer.nvim".source =
    inputs.packer;

  xdg.dataFile."nvim/site/pack/home-manager/start/paq".source =
    inputs.paq;

  xdg.dataFile."nvim/site/pack/home-manager/start/hotpot".source =
    inputs.hotpot;

  xdg.configFile."starship.toml".source =
    ../configs/starship/.config/starship.toml;

  xdg.configFile."wezterm" = {
    source = ../configs/wezterm/.config/wezterm;
    recursive = true;
  };
}

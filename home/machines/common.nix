{ lib, pkgs, ... }:

let
  channel =
    pkgs.runCommandLocal "channel" { } ''
      mkdir -p $out/channels
      ln -s ${pkgs.path} $out/channels/nixpkgs
    '';

in
{
  home.stateVersion = "22.11";

  home.packages = [
    channel
    pkgs.cached-nix-shell
    pkgs.comma
    pkgs.comma-update
    pkgs.direnv
    pkgs.fd
    pkgs.fzf
    pkgs.git
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.ghc-tags
    pkgs.home-manager
    pkgs.home-rebuild
    pkgs.iosevka-bin
    pkgs.jq
    pkgs.kakoune
    pkgs.neovim
    pkgs.nerdfonts
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.ripgrep
    pkgs.rustup
    pkgs.sd
    pkgs.shellcheck
    pkgs.starship
    pkgs.stylua
    pkgs.tealdeer
    pkgs.zoxide
  ];

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
  ];

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
    recursive = true;
  };

  xdg.configFile."hammerspoon" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../../configs/hammerspoon/.config/hammerspoon;
    recursive = true;
  };

  xdg.configFile."karabiner" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../../configs/karabiner/.config/karabiner;
    recursive = true;
  };

  xdg.configFile."kak" = {
    source = ../../configs/kakoune/.config/kak;
    recursive = true;
  };

  xdg.configFile."nvim" = {
    source = ../../configs/neovim/.config/nvim;
    recursive = true;
  };

  xdg.configFile."starship.toml".source =
      ../../configs/starship/.config/starship.toml;

  xdg.configFile."wezterm" = {
    source = ../../configs/wezterm/.config/wezterm;
    recursive = true;
  };
}

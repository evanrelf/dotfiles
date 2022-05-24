{ inputs, pkgs, ... }:

let
  channel =
    pkgs.runCommandLocal "channel" { } ''
      mkdir -p $out/channels
      ln -s ${inputs.nixpkgs} $out/channels/nixpkgs
    '';

in
{
  home.packages = [
    channel
    pkgs.comma
    pkgs.comma-update
    pkgs.direnv
    pkgs.fd
    pkgs.fzf
    pkgs.git
    pkgs.haskellPackages.fourmolu
    pkgs.helix
    pkgs.home-manager
    pkgs.home-rebuild
    pkgs.kakoune
    pkgs.neovim
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.ripgrep
    pkgs.rustup
    pkgs.sd
    pkgs.shellcheck
    pkgs.stylua
    pkgs.tealdeer
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
    {
      name = "z";
      src = pkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "z";
        rev = "45a9ff6d0932b0e9835cbeb60b9794ba706eef10";
        hash = "sha256-pWkEhjbcxXduyKz1mAFo90IuQdX7R8bLCQgb0R+hXs4=";
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

  xdg.configFile."hammerspoon" = {
    source = ../../configs/hammerspoon/.config/hammerspoon;
    recursive = true;
  };

  xdg.configFile."helix" = {
    source = ../../configs/helix/.config/helix;
    recursive = true;
  };

  xdg.configFile."karabiner" = {
    source = ../../configs/karabiner/.config/karabiner;
    recursive = true;
  };

  xdg.configFile."kak" = {
    source = ../../configs/kakoune/.config/kak;
    recursive = true;
  };

  xdg.configFile."kitty" = {
    source = ../../configs/kitty/.config/kitty;
    recursive = true;
  };

  xdg.configFile."nvim" = {
    source = ../../configs/neovim/.config/nvim;
    recursive = true;
  };
}

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
    pkgs.fd
    pkgs.fish
    pkgs.fzf
    pkgs.git
    pkgs.home-manager
    pkgs.neovim
    pkgs.ripgrep
  ];

  xdg.configFile."fish" = {
    source = ../../configs/fish/.config/fish;
    recursive = true;
  };

  xdg.configFile."git" = {
    source = ../../configs/git/.config/git;
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

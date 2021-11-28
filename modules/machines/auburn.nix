{ ... }:

{
  imports = [
    ./common.nix
    ./personal.nix
  ];

  dotfiles.programs = {
    hammerspoon.enable = true;
    karabiner.enable = true;
    kitty.enable = true;
  };
}

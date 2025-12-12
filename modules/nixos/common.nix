{ inputs, ... }:

{
  nix.settings.trusted-users = [ "@wheel" ];
  nix.settings.extra-experimental-features = [ "nix-command" "flakes" ];
}

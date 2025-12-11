{ inputs, pkgs, ... }:

{
  # TODO: Consider integrating `home-manager` into `nix-darwin`.
  # https://nix-community.github.io/home-manager/index.xhtml#sec-install-nix-darwin-module
  # imports = [
  #   "${inputs.home-manager}/nix-darwin"
  # ];

  environment.shells = [ "/Users/evanrelf/.nix-profile/bin/fish" ];

  nix.settings.experimental-features = "nix-command flakes";
  nix.settings.trusted-users = [ "@admin" ];

  nix.linux-builder.enable = true;
  nix.linux-builder.ephemeral = true;

  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
  system.stateVersion = 6;
}

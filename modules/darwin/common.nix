{ inputs, pkgs, ... }:

{
  nix.settings.experimental-features = "nix-command flakes";
  nix.settings.trusted-users = [ "@admin" ];

  nix.linux-builder.enable = true;
  nix.linux-builder.ephemeral = true;

  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
  system.stateVersion = 6;
}

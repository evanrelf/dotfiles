{ config, ... }:

{
  imports = [
    ../common.nix
  ];

  # TODO: Reinstall Lix.
  ids.gids.nixbld = 30000;

  # TODO: This machine still has the Determinate Nix installer and its repair
  # launch daemon (`systems.determinate.nix-installer.nix-hook`), which prepends
  # text to shell profiles on reboot.
  #
  # For now, these hashes tell `nix-darwin` these modifications are okay to
  # overwrite. In the future, it would be better to clean up this hodgepodge of
  # Nix installs so this hack isn't necessary.
  environment.etc."bashrc".knownSha256Hashes = [ "b67c9ea69b96f0ba89c575ddf9631d556d682ce1a3203cf0a458e368659f20ee" ];
  environment.etc."zshrc".knownSha256Hashes = [ "27274e44b88a1174787f9a3d437d3387edc4f9aaaf40356054130797f5dc7912" ];
  environment.etc."zshenv".knownSha256Hashes = [ "e7a5ba03dc84e14c8c9028888a4278c03d3f23e528b8424ab24d67aae93d59fa" ];

  system.stateVersion = 6;
}

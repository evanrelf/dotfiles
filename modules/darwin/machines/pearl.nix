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
  environment.etc."bashrc".knownSha256Hashes = [ "a2396f60e669a9e60ca5a077ee74bff1d071a4ade9f37836aa7e852ca389c243" ];
  environment.etc."zshrc".knownSha256Hashes = [ "2a36d13c46cab744f1cc9d55a21b5e944e87e7b40132a0bcab98222a69b1071f" ];
  environment.etc."zshenv".knownSha256Hashes = [ "ad03552f1fffa6e2a84d6409bb1063f5fe121f3ee4a05ef60fd0c9e493d59b8d" ];
}

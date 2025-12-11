{ config, ... }:

{
  imports = [
    ../common.nix
  ];

  # TODO: Reinstall Lix.
  ids.gids.nixbld = 30000;
}

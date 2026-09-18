# This overlay exists entirely so I can get the fix to this bug that drives me
# absolutely insane when using Fish.
#
# https://github.com/fish-shell/fish-shell/commit/c8d7476576f1859d5a7e0d294328080f3fe8fb10
#
# TODO: Once my Nixpkgs pin gets v4.8.0 or newer, I should delete this.

final: prev:

{
  fish = final.callPackage "${final.inputs.nixpkgs-master}/pkgs/by-name/fi/fish/package.nix" { };
}

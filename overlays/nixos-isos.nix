pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev.inputs) nixpkgs;

  mkIso = modules:
    (nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      inherit modules;
    }).config.system.build.isoImage;

  minimalConfiguration = { ... }: {
    imports = [
      "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
      "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
    ];
  };

  evanConfiguration = { lib, pkgs, ... }: {
    # Configure SSH daemon and root password for zero-setup remote installation
    systemd.services.openssh.wantedBy = lib.mkForce [ "multi-user.target" ];
    services.openssh.ports = [ 2222 ];
    users.users.root.initialPassword = "alpine";

    environment.systemPackages = [ pkgs.kakoune ];
  };

in
{
  nixosIsos = {
    minimal = mkIso [ minimalConfiguration ];

    evan = mkIso [
      minimalConfiguration
      evanConfiguration
    ];
  };
}

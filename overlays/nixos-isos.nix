pkgsFinal: pkgsPrev:

let
  inherit (pkgsPrev.inputs) nixpkgs;

  mkIso = args@{ system ? "x86_64-linux", modules, ... }:
    (nixpkgs.lib.nixosSystem args).config.system.build.isoImage;

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
    minimal = mkIso {
      modules = [ minimalConfiguration ];
    };

    minimal-arm = mkIso {
      system = "aarch64-linux";
      modules = [ minimalConfiguration ];
    };

    evan = mkIso {
      modules = [
        minimalConfiguration
        evanConfiguration
      ];
    };
  };
}

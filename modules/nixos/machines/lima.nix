{ inputs, lib, modulesPath, pkgs, ... }:

{
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
    inputs.nixos-lima.nixosModules.lima
    ../common.nix
  ];

  boot.kernelParams = [ "console=tty0" ];
  boot.loader.grub = {
    device = "nodev";
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  fileSystems."/boot" = {
    device = lib.mkForce "/dev/vda1"; # /dev/disk/by-label/ESP
    fsType = "vfat";
  };
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    autoResize = true;
    fsType = "ext4";
    options = [ "noatime" "nodiratime" "discard" ];
  };

  environment.systemPackages = with pkgs; [ kakoune ];

  services.openssh.enable = true;
  services.lima.enable = true;

  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "25.11";
}

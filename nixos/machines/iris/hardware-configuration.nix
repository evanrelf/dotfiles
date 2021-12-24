{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sr_mod" ];

  boot.kernelModules = [ "kvm-amd" ];

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/66C8-88A2";
      fsType = "vfat";
    };

  fileSystems."/" =
    {
      device = "tank/local/root";
      fsType = "zfs";
      # options = [ "zfsutil" ];
    };

  fileSystems."/nix" =
    {
      device = "tank/local/nix";
      fsType = "zfs";
      # options = [ "zfsutil" ];
    };

  fileSystems."/home" =
    {
      device = "tank/safe/home";
      fsType = "zfs";
      # options = [ "zfsutil" ];
    };

  fileSystems."/persist" =
    {
      device = "tank/safe/persist";
      fsType = "zfs";
      # options = [ "zfsutil" ];
    };

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}

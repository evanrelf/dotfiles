{ inputs, lib, pkgs, ... }:

{
  imports = [
    ../common.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "nvme" "usbhid" "sr_mod" ];
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "12345678";
  boot.zfs.devNodes = "/dev/nvme0n1p2";

  fileSystems."/boot" = {
    fsType = "vfat";
    device = "/dev/nvme0n1p1";
  };
  fileSystems."/" = {
    fsType = "zfs";
    device = "tank/local/root";
  };
  fileSystems."/nix" = {
    fsType = "zfs";
    device = "tank/local/nix";
  };
  fileSystems."/persist" = {
    fsType = "zfs";
    device = "tank/safe/persist";
  };
  fileSystems."/home" = {
    fsType = "zfs";
    device = "tank/safe/home";
  };

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r tank/local/root@blank
  '';

  environment.etc = {
    "nixos".source = "/persist/etc/nixos";
    "adjtime".source = "/persist/etc/adjtime";
    "NIXOS".source = "/persist/etc/NIXOS";
  };

  security.sudo.extraConfig = ''
    Defaults lecture = never
  '';

  virtualisation.vmware.guest.enable = true;

  programs.fish.enable = true;

  users.users.evanrelf = {
    isNormalUser = true;

    extraGroups = [ "wheel" ];

    initialPassword = "banana";

    shell = pkgs.fish;

    packages = [
      pkgs.cowsay
      pkgs.kakoune
    ];
  };

  system.stateVersion = "25.11";
}

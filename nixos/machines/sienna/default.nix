{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
  ];
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  fileSystems = {
    "/" =
      { device = "rpool/local/root"; fsType = "zfs"; };
    "/boot" =
      { device = "/dev/disk/by-uuid/4B03-3702"; fsType = "vfat"; };
    "/nix" =
      { device = "rpool/local/nix"; fsType = "zfs"; };
    "/home" =
      { device = "rpool/safe/home"; fsType = "zfs"; };
    "/persist" =
      { device = "rpool/safe/persist"; fsType = "zfs"; };
  };
  swapDevices = [
    { device = "/dev/disk/by-uuid/0905e0c9-1508-49c1-94ee-f912247550d7"; }
  ];
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.video.hidpi.enable = lib.mkDefault true;
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # LUKS
  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/7020fe63-8f13-4c52-b813-c1746130319a";

  # ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "340297d5";
  boot.zfs.devNodes = "/dev/vg/root";
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;

  # # Scheduled ZFS snapshots
  # services.sanoid.enable = true;
  # services.sanoid.datasets =
  #   let
  #     common = {
  #       autosnap = true;
  #       autoprune = true;
  #       hourly = 48;
  #       daily = 30;
  #       monthly = 0;
  #       yearly = 0;
  #     };
  #   in
  #   {
  #     "rpool/safe/home" = common;
  #     "rpool/safe/persist" = common;
  #   };

  # # Backups
  # services.restic.backups."b2" = {
  #   repository = "b2:evanrelf-backup:sienna";
  #   initialize = true;
  #   passwordFile = "/etc/restic/b2/password";
  #   environmentFile = "/etc/restic/b2/environment";
  #   paths = [
  #     "/home"
  #     "/persist"
  #   ];
  #   pruneOpts = [
  #     "--keep-daily 7"
  #     "--keep-weekly 8"
  #     "--keep-monthly 12"
  #     "--keep-yearly 1"
  #   ];
  # };

  # # Roll back to blank snapshot on boot
  # boot.initrd.postDeviceCommands = lib.mkAfter ''
  #   zfs rollback -r rpool/local/root@blank
  # '';

  # # Persist state
  # fileSystems."/persist".neededForBoot = true;
  # environment.persistence."/persist" = {
  #   directories = [
  #     "/etc/NetworkManager/system-connections"
  #     "/etc/nixos"
  #     "/etc/restic/b2"
  #   ];
  #   files = [
  #     "/etc/machine-id"
  #     "/etc/ssh/ssh_host_ed25519_key"
  #     "/etc/ssh/ssh_host_ed25519_key.pub"
  #     "/etc/ssh/ssh_host_rsa_key"
  #     "/etc/ssh/ssh_host_rsa_key.pub"
  #     "/var/lib/NetworkManager/secret_key"
  #     "/var/lib/NetworkManager/seen-bssids"
  #     "/var/lib/NetworkManager/timestamps"
  #     "/var/lib/tailscale/tailscaled.state"
  #   ];
  # };

  security.sudo.extraConfig = "Defaults lecture = never";

  networking.hostName = "sienna";
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  users.users.root.initialPassword = "alpine";
  users.users.evan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
    initialPassword = "banana";
  };

  environment.systemPackages = [
    pkgs.kakoune
    # pkgs.restic
  ];

  nix.settings.trusted-users = [ "root" "@wheel" ];
  nix.setting.max-jobs = 16;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  services.openssh.enable = true;
  # services.tailscale.enable = true;

  programs.ssh.startAgent = true;
  programs.gnupg.agent.enable = true;

  time.timeZone = "America/Los_Angeles";

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

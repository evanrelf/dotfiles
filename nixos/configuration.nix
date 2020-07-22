{ config, lib, pkgs, ... }:

{
  imports = [
    <nixos-hardware/lenovo/thinkpad/x1/6th-gen>
  ];

  # KERNEL
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];

  # BOOT
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # FILESYSTEMS
  boot.supportedFilesystems = [ "zfs" ];
  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/d64ccaed-c06d-4859-88b4-d1876270c9b9";
  boot.zfs.devNodes = "/dev/vg/root";
  boot.zfs.forceImportAll = false;
  networking.hostId = "978145c5";
  boot.tmpOnTmpfs = true;
  fileSystems = {
    "/boot" = { device = "/dev/disk/by-uuid/197C-84DE"; fsType = "vfat"; };
    "/" = { device = "rpool/local/root"; fsType = "zfs"; };
    "/nix" = { device = "rpool/local/nix"; fsType = "zfs"; };
    "/persist" = { device = "rpool/safe/persist"; fsType = "zfs"; };
    "/home" = { device = "rpool/safe/home"; fsType = "zfs"; };
  };
  swapDevices = [
    { device = "/dev/disk/by-uuid/96a91dd6-c5e6-4e87-a90a-4b960ae6a42d"; }
  ];
  services.zfs.trim.enable = true;
  services.zfs.autoScrub = {
    enable = true;
    interval = "daily";
  };

  # DESKTOP
  services.xserver.enable = true;
  services.xserver.displayManager.startx.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  # INPUT DEVICES
  services.xserver.libinput = {
    enable = true;
    naturalScrolling = true;
    tapping = false;
    tappingDragLock = false;
    middleEmulation = false;
    accelSpeed = "0.7";
  };
  systemd.services.keyswap = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      RemainAfterExit = "yes";
      Type = "oneshot";
    };
    script = ''
      PATH=/run/current-system/sw/bin:$PATH
      setkeycodes 3a 1
      setkeycodes 38 125
      setkeycodes e05b 56
    '';
  };
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    sensitivity = 200; # 0-255, default 128
    speed = 125; # 0-255, default 97
  };

  # VIDEO
  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''Option "TearFree" "true"'';
  programs.light.enable = true;

  # SOUND
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # NETWORKING
  networking.hostName = "sienna";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  # POWER
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  services.undervolt = {
    enable = true;
    coreOffset = "-50";
    gpuOffset = "-50";
  };
  networking.networkmanager.wifi.powersave = true;

  # SECURITY
  services.openssh.enable = true;
  programs.gnupg.agent.enable = true;
  services.physlock.enable = true;
  hardware.u2f.enable = true;

  # NIX
  nix.trustedUsers = [ "root" "@wheel" ];
  nix.maxJobs = 8;
  nixpkgs.config.allowUnfree = true;

  # PACKAGES
  programs.fish.enable = true;
  environment.systemPackages = with pkgs; [
    binutils
    firefox
    kitty
    neovim
    tmux
  ];

  # FONTS
  fonts.enableDefaultFonts = true;
  console.earlySetup = true;
  console.font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # HARDWARE
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  # USERS
  users.mutableUsers = false;
  users.users.evanrelf = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
    initialPassword = "banana";
    shell = pkgs.fish;
  };

  # STATE
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';
  environment.etc = {
    "nixos".source = "/persist/etc/nixos";
    "machine-id".source = "/persist/etc/machine-id";
    "NetworkManager/system-connections".source = "/persist/etc/NetworkManager/system-connections";
    "ssh/ssh_host_ed25519_key".source = "/persist/etc/ssh/ssh_host_ed25519_key";
    "ssh/ssh_host_ed25519_key.pub".source = "/persist/etc/ssh/ssh_host_ed25519_key.pub";
    "ssh/ssh_host_rsa_key".source = "/persist/etc/ssh/ssh_host_rsa_key";
    "ssh/ssh_host_rsa_key.pub".source = "/persist/etc/ssh/ssh_host_rsa_key.pub";
  };
  systemd.tmpfiles.rules = [
    "L /var/lib/NetworkManager/secret_key - - - - /persist/var/lib/NetworkManager/secret_key"
    "L /var/lib/NetworkManager/seen-bssids - - - - /persist/var/lib/NetworkManager/seen-bssids"
    "L /var/lib/NetworkManager/timestamps - - - - /persist/var/lib/NetworkManager/timestamps"
  ];
  security.sudo.extraConfig = "Defaults lecture = never";

  # MISCELLANEOUS
  services.printing.enable = true;
  time.timeZone = "America/Los_Angeles";
  system.stateVersion = "20.03";
}

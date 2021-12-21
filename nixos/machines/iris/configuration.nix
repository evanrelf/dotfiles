{ config, lib, pkgs, ... }:

{
  hardware.enableRedistributableFirmware = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # LUKS
  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/242c09c6-51f4-4fae-911e-c0933c6a1f8d";

  # ZFS
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "d9f31fe6";
  boot.zfs.devNodes = "/dev/vg/root";
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;

  # Scheduled ZFS snapshots
  services.sanoid.enable = true;
  services.sanoid.datasets =
    let
      common = {
        autosnap = true;
        autoprune = true;
        hourly = 48;
        daily = 30;
        monthly = 0;
        yearly = 0;
      };
    in
    {
      "rpool/safe/home" = common;
      "rpool/safe/persist" = common;
    };

  # Backups
  services.restic.backups."b2" = {
    repository = "b2:evanrelf-backup:iris";
    initialize = true;
    passwordFile = "/etc/restic/b2/password";
    environmentFile = "/etc/restic/b2/environment";
    paths = [
      "/home"
      "/persist"
    ];
    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 8"
      "--keep-monthly 12"
      "--keep-yearly 1"
    ];
  };

  # Roll back to blank snapshot on boot
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';

  # Persist state
  fileSystems."/persist".neededForBoot = true;
  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/nixos"
      "/etc/restic/b2"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/var/lib/NetworkManager/secret_key"
      "/var/lib/NetworkManager/seen-bssids"
      "/var/lib/NetworkManager/timestamps"
      "/var/lib/tailscale/tailscaled.state"
    ];
  };

  security.sudo.extraConfig = "Defaults lecture = never";

  networking.hostName = "iris";
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  networking.useDHCP = false;
  networking.interfaces.enp42s0.useDHCP = true;
  networking.interfaces.wlp41s0.useDHCP = true;

  users.users.root.initialPassword = "alpine";
  users.users.evan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
    initialPassword = "banana";
  };

  environment.systemPackages = [
    pkgs.kakoune
    pkgs.restic
  ];

  nix.trustedUsers = [ "root" "@wheel" ];
  nix.maxJobs = 16;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  services.openssh.enable = true;
  services.tailscale.enable = true;

  programs.ssh.startAgent = true;
  programs.gnupg.agent.enable = true;

  time.timeZone = "America/Los_Angeles";


  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

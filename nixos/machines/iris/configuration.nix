{ config, lib, pkgs, ... }:

{
  hardware.enableRedistributableFirmware = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # LUKS
  boot.initrd.luks = {
    devices = {
      "crypt0" = {
        device = "/dev/disk/by-uuid/6a553fcf-5a9f-490b-98fe-5c830e5ba862";
        bypassWorkqueues = true;
      };
      "crypt1" = {
        device = "/dev/disk/by-uuid/a0d1e98d-6ee7-4d2f-bbb6-8266e6714be6";
        bypassWorkqueues = true;
      };
    };
    reusePassphrases = true;
  };

  # ZFS
  boot.zfs.devNodes = "/dev/mapper";
  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  networking.hostId = "89922804";

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
      "tank/safe/home" = common;
      "tank/safe/persist" = common;
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
    zfs rollback -r tank/local/root@blank
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

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  services.tailscale.enable = true;
  networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];

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

  programs.ssh.startAgent = true;
  programs.gnupg.agent.enable = true;

  time.timeZone = "America/Los_Angeles";

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

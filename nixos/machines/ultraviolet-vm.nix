{ config, lib, modulesPath, pkgs, ... }:

{
  # Hardware
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];
  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS
  boot.zfs.devNodes = "/dev/disk/by-uuid/11341917885459316852";
  networking.hostId = "d684c2fe";

  # Networking
  networking.hostName = "ultraviolet-vm";
  services.openssh.enable = true;

  # Users
  users.mutableUsers = false;
  users.users.evanrelf = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
    initialPassword = "banana";
  };

  environment.systemPackages = [
    pkgs.file
    pkgs.neovim
  ];

  nix.settings.trusted-users = [ "root" "@wheel" ];
  nix.settings.max-jobs = 8;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.fish.enable = true;
  programs.fish.useBabelfish = true;

  programs.ssh.startAgent = true;

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "tty";

  time.timeZone = "America/Los_Angeles";

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
